{- |
  Parsers for the power system data format provided by SEV.
-}
module IO.Parse.Measurement.SEV
(
-- * Parsing data from SEV
  parseSEV
)
where


-- List processing:
import Data.List (intersectBy,sort)

-- Literally magic:
import Control.Arrow ((***))

-- Vectors:
import Util.Vector (Vector)
import qualified Util.Vector as V
import qualified Util.Matrix as M

-- Maps:
import Data.Map (Map)
import qualified Data.Map as I

-- Dates:
import Util.Time
  (Day, UTCTime (..), secondsToDiffTime, utcTimeToPOSIXTimestamp)

-- Local:
import IO.Parse.Util as P8
import Natural.Measurement.SEV



-- Parse a SEV data file.
-- | A Parser which returns a `SEVData` with the data from the headers in
-- the argument.
parseSEV :: Day
         -> [String] -- ^ The measurement location names to project out.
         -> Parser SEVData
parseSEV day targets = do
  skipLines 9
  valNums <- parseHeaders targets
  skipLines 3
  (valTimes,valData) <- parseData day (sort $ I.elems valNums)
  return SEVData
    { sevData = M.reshape (I.size valNums) valData
    , sevTimes = valTimes
    , sevHeaders = valNums
    }



-- The header section.
-- | Parse the header section, returning the entry names in order.
parseHeaders :: [String] -> Parser (Map String Int)
parseHeaders targets = makeMap targets <$> many parseHeader

-- | Parse a header entry, extracting the name and its value number.
parseHeader :: Parser String
parseHeader =
     string " VAL"
  *> takeTill (==':')
  *> P8.take 2
  *> manyTill anyChar (string "  ")
  <* skipLines 2

-- | Make a map from value name to value column in data file.
makeMap :: [String] -> [String] -> Map String Int
makeMap ts hs = I.fromList $ intersectBy (\(s1,_) (s2,_) -> s1==s2) zhs zts
  where
    zhs = Prelude.zip hs [1..]
    zts = Prelude.zip ts [1..]



-- Parsing the data section.
-- | Parse the data section of the SEV file.
parseData :: Day -> [Int] -> Parser (Vector Int, Vector Double)
parseData date idxs =
  (V.fromList *** V.concat) <$> unzip <$> many (parseEntry date idxs)

-- | Parse an entry in the data section.
parseEntry :: Day -> [Int] -> Parser (Int, Vector Double)
parseEntry date idxs = do
  -- Optional date field (everything is in a day, so ignore).
  _ <- P8.take 12
  -- Time of day
  h <- decimal
  _ <- char ':'
  m <- decimal
  _ <- char ':'
  s <- decimal
  let dt = secondsToDiffTime $ h*3600 + m*60 + s
  skipSpace
  entry <- projectColumns idxs
  return (utcTimeToPOSIXTimestamp $ UTCTime date dt, entry)

-- | Project out the right columns from the SEV file.
projectColumns :: [Int] -> Parser (Vector Double)
projectColumns idxs = genVec skips V.empty
  where
    genVec [] vec = do
      skipLine
      return vec
    genVec (s:ss) vec = do
      _ <- count s skipVal
      val <- double
      skipSpace
      genVec ss $ V.snoc vec val
    skips = head idxs-1:zipWith (\x y -> x-y-1) (tail idxs) (init idxs)

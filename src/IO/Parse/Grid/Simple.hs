{- |
  Facilities for parsing power system data from text in the custom format
  used for this program, i.e. kind of the reciprocal to `Text.Render`.
-}
module IO.Parse.Grid.Simple
(
-- * Grid parsing
  parseGrid

-- * Line parsing
, parseLine
, parseLines

-- * Bus parsing
, parseBus
, parseBuses
) where



-- Local:
import IO.Parse.Util
import Data.Grid.Simple



-- Grid parsing
-- | Parse a `Grid`.
parseGrid :: Parser Grid
parseGrid = do
  skipLine
  name <- parseString <* skipLine
  skipLines 2
  base <- double <* skipLine
  skipLines 3
  bs <- parseBuses
  skipLines 3
  gs <- parseGens
  skipLines 3
  ls <- parseLines
  return Grid
    { gridName = name
    , gridMVAbase = base
    , gridBuses = bs
    , gridGens = gs
    , gridLines = ls
    }



-- Bus parsing.
-- | Parse a series of `Buses` from text.
parseBuses :: Parser Buses
parseBuses = many $ parseBus <* endOfLine

-- | Parse a single `Bus` from text.
parseBus :: Parser SBus
parseBus = do
  name <- parseString
  sep
  bID <- decimal
  sep
  bPow <- complexDouble
  sep
  bAdm <- complexDouble
  sep
  v <- complexDouble
  sep
  vBase <- double
  return SBus
    { sbusName=name
    , sbusID=bID
    , sbusPower=bPow
    , sbusAdmittance=bAdm
    , sbusVoltage=v
    , sbusVoltageBase=vBase }



-- Generator parsing.
-- | Parse a series of `Gens` from text.
parseGens :: Parser Gens
parseGens = many $ parseGen <* skipLine

-- | Parse a `SGen` from text.
parseGen :: Parser SGen
parseGen = do
  bID <- decimal
  sep
  pq <- complexDouble
  sep
  sMax <- complexDouble
  sep
  sMin <- complexDouble
  sep
  v <- double
  return $ SGen bID pq sMax sMin v



-- Line parsing.
-- | Parse a series of `Line`s from text.
parseLines :: Parser Lines
parseLines = many $ parseLine <* endOfLine

-- | Parse a `Line` from text.
parseLine :: Parser SLine
parseLine = do
  lID <- decimal
  sep
  lFrom <- decimal
  sep
  lTo <- decimal
  sep
  lImpedance <- complexDouble
  sep
  lSusceptance <- double
  return $ SLine lID lFrom lTo lImpedance lSusceptance

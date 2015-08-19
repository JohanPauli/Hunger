{- |
  Utility functions for parsing. Things like skipping lines and separators.
-}
module IO.Parse.Util
(
-- * Custom utilities
  sep
, comment
, parseString
, complexDouble
, takeLine
, skipLine
, skipLines
, skipVal
, skipVals

-- * Also exports parsing library
, module Data.Attoparsec.ByteString.Char8
, module B8
, module Control.Applicative
) where


-- Parsing:
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as P8

-- Bytestrings:
import Data.ByteString.Char8 as B8 (pack,unpack)

-- Control:
import Control.Applicative
import Control.Monad (void)

-- Complex numbers:
import Data.Complex



-- Utilities.
-- | Disgards separator character.
sep :: Parser ()
sep = skipWhile (==' ')

-- | Comment skipper.
comment :: Parser ()
comment = string "--" >> skipLine

-- | Parse a string encased in
parseString :: Parser String
parseString = do
  _ <- char '"'
  str <- takeTill (=='"')
  _ <- char '"'
  return (unpack str)

-- | Parse a complex number as a double.
complexDouble :: Parser (Complex Double)
complexDouble = do
  a <- double
  _ <- char '+'
  b <- double
  _ <- char 'i'
  return (a :+ b)

-- | Take a line.
takeLine :: Parser String
takeLine = unpack <$> P8.takeWhile (\x -> x/='\n' && x/='\r') <* endOfLine

-- | Skip a line.
skipLine :: Parser ()
skipLine = skipWhile (\x -> x/='\n' && x/='\r') *> endOfLine

-- | Skips a number of lines.
skipLines :: Int -> Parser ()
skipLines n = void $ P8.count n skipLine

-- | Skip an entry.
skipVal :: Parser ()
skipVal = skipSpace >> double >> skipSpace

-- | Skip a number of entries.
skipVals :: Int -> Parser ()
skipVals n = void $ P8.count n skipVal

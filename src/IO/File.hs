{- |
  A set of functions for rendering and parsing text files with power system
  data.

  Generally uses `Parser` and `Renderer` to get/put strings.
-}
module IO.File
(
-- * Rendering files
  renderFile

-- * Parsing files
, parseFile
, parseFile'
) where



-- Strings:
import Data.ByteString.Char8 as B8

-- Local:
import IO.Render.Util (Renderer)
import IO.Parse.Util (Parser, parseOnly)



-- Rendering.
-- | Render a data structure to a file, using a rendering function.
renderFile :: FilePath -> Renderer a -> a -> IO ()
renderFile fp f dat = B8.writeFile fp (f dat)



-- Parsing.
-- | Parse a data structure from a file, using a parser.
--
-- This version fails with an `error` if the parse fails.
parseFile :: FilePath -> Parser a -> IO a
parseFile fp p = do
  file <- B8.readFile fp
  case parseOnly p file of
    Left er -> error er
    Right r -> return r

-- | Parse a data structure from a file, using a parser.
--
-- This version gives an `Either`, to handle errors.
parseFile' :: FilePath -> Parser a -> IO (Either String a)
parseFile' fp p = do
  file <- B8.readFile fp
  return $ parseOnly p file

{- |
  Tests for loading and saving data using parsers and renderers.
-}
module Test.LoadSave
(
-- * Test loading and saving capabilities.
  testLoadSave
)
where



-- Local:
import IO.File (parseFile, renderFile)
import IO.Parse (parseMCF, parseSGrid)
import IO.Render (unpack, renderSGrid)



-- | Load and save some data using parsers/renderers.
testLoadSave :: IO ()
testLoadSave = do
  putStrLn "\nGrid data load/save test...\n"

  putStrLn "Loading MCF case 5 grid..."
  grid <- parseFile "case5.m" parseMCF

  putStrLn "Saving the grid to 'case5.dat'..."
  renderFile "case5.dat" renderSGrid grid

  putStrLn "Loading standard grid from 'case5.dat'..."
  gridStd <- parseFile "case5.dat" parseSGrid

  putStrLn "Printing grid:"
  (putStrLn . unpack . renderSGrid) gridStd

{- |
  Tests for calculating and displaying grid statistics.
-}
module Test.Statistics
(
  testStats
) where



-- Local:
import IO.File (parseFile)
import IO.Parse.Grid.MCF (parseMCF)
import IO.Render.Util (unpack)
import IO.Render.Grid.Simple (renderGrid)



-- | Test the statistics functionality.
testStats :: String -> IO ()
testStats fp = do
  putStrLn $ "\nTesting statistics for " ++ fp ++ "...\n"

  putStrLn "Loading a grid:"
  grid <- parseFile fp parseMCF
  putStrLn $ unpack $ renderGrid grid

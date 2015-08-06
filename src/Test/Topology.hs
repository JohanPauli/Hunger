{- |
  Simple tests of the topological functions.
-}
module Test.Topology where



-- IntMap, for IntMapping:
import Data.IntMap as M

-- Local:
import Data.Grid.Topology



testTop :: IO ()
testTop = do
  putStrLn "Testing the default topology:"
  let top = defaultTopology
  print top
  putStrLn ""

  putStrLn "Joining edges to themselves:"
  print $ joinEdges top (edges top)

  putStrLn "Relabeling edges in opposite order:"
  let ids = (fmap nodeID . nodes) top
      newLabels = M.fromList $ zip ids (reverse ids)
  print $ relabel newLabels (nodes top) (edges top)

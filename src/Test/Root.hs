{- |
  Test of the root-finding algorithms.
-}
module Test.Root where



-- Vectors and matrices:
import Util.Vector (Vector)
import qualified Util.Vector as V
import Util.Matrix ((#>))
import qualified Util.Matrix as M

-- Root-finding stuff:
import Util.Root



-- | The function for which to find the root.
f :: Vector Double -> Vector Double
f x = m #> x - b
  where
    m = M.fromLists
      [[4, 1,-1]
      ,[2, 7, 1]
      ,[1,-3, 12]]

b :: Vector Double
b = V.fromList [3,19,31]

x0 :: Vector Double
x0 = V.fromList [0,0,0]


testRoot :: IO ()
testRoot = do
  putStrLn "\nTesting root-finding-algorithm...\n"

  putStrLn "Using Newton-Raphson:"
  M.disp 2 $ M.fromColumns $ root QuasiNewton 1e-4 f x0

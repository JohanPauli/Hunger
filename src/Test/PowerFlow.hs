{- |
  A test of the power flow solution algorithm.
-}
module Test.PowerFlow
where



-- IntMapping:
import qualified Data.IntMap as M

-- Electrical types and matrices:
import qualified Util.Matrix as V

-- Interfaces:
import Interface.Topology
import Interface.Grid

-- I/O operations and stuff:
import IO.File
import IO.Render
import IO.Parse

-- Construction of power flow data structures:
import Conversion.Grid

-- Power flow and grid statistics:
import Statistics.PowerFlow

-- The tested data structure:
import Natural.Simple

-- The tested analysis:
import Modelling.PowerFlow



-- | Test power flow solvers on MatPower a case.
testPowerFlow :: FilePath -> IO ()
testPowerFlow fp = do
  putStrLn "\nPower flow test...\n"

  putStrLn $ "Loading " ++ fp ++ "..."
  grid <- normalize <$> parseFile fp parseMCF
  putStrLn $ unpack $ renderSGrid grid

  putStrLn "Labeling buses:"
  let classes = classifyBuses (gridBuses grid) (gridGens grid)

  putStrLn "Sorting buses:"
  let shifts = nodeMap classes
  print shifts
  putStrLn ""

  putStrLn "Unsorted [Y]:"
  let adm = makeY
              (M.fromList [(i,i) | i <- [1..length classes]])
              grid
              (gridLines grid)
              (gridBuses grid)
  putStrLn $ V.dispcf 4 adm


  putStrLn "Unsorted [S]:"
  putStrLn $ V.dispcf 2 $ V.asColumn $
    makeS
      (M.fromList [(i,i) | i <- [1..length classes]])
      (gridGens grid)
      (gridBuses grid)

  putStrLn "Unsorted [V]:"
  putStrLn $ V.dispcf 2 $ V.asColumn $
    makeV
      (M.fromList [(i,i) | i <- [1..length classes]])
      (gridBuses grid)
      (gridGens grid)

  putStrLn "Test of Jacobi PF:"
  let resJC =
        solvePF
          Jacobi
          grid
          (gridBuses grid)
          (gridGens grid)
          (gridBuses grid)
          (gridLines grid)
          (gridBuses grid)
  (putStrLn . unpack) $ renderPF
    (gridMVAbase grid)
    (pfStats grid (gridBuses grid) (gridLines grid) resJC)

  putStrLn "Test of Gauss-Seidel PF:"
  let resGS =
        solvePF
          GaussSeidel
          grid
          (gridBuses grid)
          (gridGens grid)
          (gridBuses grid)
          (gridLines grid)
          (gridBuses grid)
  (putStrLn . unpack) $ renderPF
    (gridMVAbase grid)
    (pfStats grid (gridBuses grid) (gridLines grid) resGS)

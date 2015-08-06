{- |
  A program for simulating Power Systems (also known as Grids).

  The main portion of the program encapsulates configuration and the
  execution of configured actions.
-}
module Main where



-- Command line inputs:
import System.Environment

-- Control:
import Control.Monad

-- Natural data structures:
import Natural.Config
import Natural.Simple

-- Interfaces:
import Interface.Grid

-- Statistics:
import Statistics.PowerFlow

-- Analysis functions:
import Modelling.PowerFlow

-- IO stuff:
import IO.File
import IO.Parse
import IO.Render

-- Testing:
import Test.LoadSave
import Test.PowerFlow



-- | The main program, only tests currently.
main :: IO ()
main = do
  testLoadSave
  testPowerFlow "case5.m"

  -- Fetch command-line arguments, parse the config from file (if any),
  -- and process the config (performing analyses, etc.).
  process =<< getConfig =<< getArgs

-- | Parse the config file, if supplied
getConfig :: [String] -> IO Config
getConfig [] = return defaultConfig
getConfig (fp:_) = parseFile fp parseConfig

-- | Clumsy case-by-case processing of the config.
--
-- This is absolutely not the right way to do this, as it would
-- necessitate a whole separate implementation for each
-- natural representation.
--
-- This is a hack and the problem it solves can still be considered an
-- something unsolved.
process :: Config -> IO ()
process Config
  { confGridFile=fp
  , confGridFormat=MCF
  , confAnalyses=analyses
  } = do
  putStrLn $ "Parsing a grid from " ++ fp ++ "\n"
  -- Parse and normalize grid.
  g <- liftM normalize $ parseFile fp parseMCF
  (putStrLn . unpack . renderStats) g
  sequence_ $ fmap (analyseSimple g) analyses
process Config{confGridFormat=_} =
  error "Only MCF grids are implemented"


-- | Analyse a the Simple type of grid from `Data.Grid.Simple`.
analyseSimple :: SGrid -> Analysis -> IO ()
analyseSimple g@SGrid
  { gridMVAbase=base
  , gridBuses=bs
  , gridGens=gs
  , gridLines=ls
  } PowerFlowGS = do
  putStrLn "Performing Gauss-Seidel power flow:"
  let stats = pfStatsSolve GaussSeidel g bs gs bs ls bs
  (putStrLn . unpack . renderStats) g
  (putStrLn . unpack . renderPF base) stats
analyseSimple g@SGrid
  { gridMVAbase=base
  , gridBuses=bs
  , gridGens=gs
  , gridLines=ls
  } PowerFlowJC = do
  putStrLn "Performing Jacobi power flow:"
  let stats = pfStatsSolve Jacobi g bs gs bs ls bs
  (putStrLn . unpack . renderPF base) stats

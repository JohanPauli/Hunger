{- |
  Renders statistics for a `Simple` grid.
-}
module IO.Render.Statistics.Simple
(
-- * Statistics for a power system
  renderStats
)
where



-- Printing with f:
import Text.Printf

-- Local:
import Data.Grid.Types
import IO.Render.Util
import Data.Grid.Simple
import Analysis.Statistics.Grid



-- | Compute and render statistics for a grid.
renderStats :: Renderer Grid
renderStats Grid
  { gridName=name
  , gridMVAbase=base
  , gridBuses=bs
  , gridGens=gs
  , gridLines=ls
  } =
     "Grid: " <> pack name <> " with MVA base " <> toBS base <> " MVA" <> endl
  <> pack (printf "%15s  %20s %10s %10s\n"
        ("Counts:"::String) ("Sizes:"::String)
        ("P (MW)"::String) ("Q (MVar)"::String))
  <> addLine "Buses" (count bs) "Total Gen Capacity" pTot qTot
  <> addLine "Generators" (count gs) "Generation" pNow qNow
  <> addLine "Loads" nLoad "Load" pLoad qLoad
  <> pack (printf "%15s%5d" ("Shunts"::String) nShunt) <> endl
  <> pack (printf "%15s%5d" ("Lines"::String) (count ls)) <> endl
  where
    pTot :+ qTot = total sgenMax gs * (base:+0)
    pNow :+ qNow = total sgenPower gs * (base:+0)
    pLoad :+ qLoad = total sbusPower bs * (base:+0)
    nShunt = count $ filter (\x -> sbusAdmittance x/=0:+0) bs
    nLoad = count $ filter (\x -> sbusPower x/=0:+0) bs

-- | Utility function for printing lines of data.
addLine :: String -> Int -> String -> Double -> Double -> ByteString
addLine a b c d e = pack $ printf "%15s%5d%20s%10.2f%10.2f\n" a b c d e

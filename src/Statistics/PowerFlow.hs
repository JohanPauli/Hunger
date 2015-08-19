{- |
  Derivation and representation of information that can be derived from
  power flow results.
-}
module Statistics.PowerFlow
(
-- * Types
  PFStats (..)
, BusFlow (..)
, LineFlow (..)

-- * Calculating Statistics
, pfStatsSolve
, pfStats
, busFlows
, lineFlows
)
where



-- IntMapping:
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

-- Electrical types and Vectors/Matrices:
import Util.Types
import Util.Vector (Vector)
import Util.Matrix (Matrix, (#>), (!), conj)

-- Interfaces:
import Interface.Topology
import Interface.Node
import Interface.Edge

-- Solving power flows:
import Modelling.PowerFlow



-- | Power flow statistics are bus and line statistics, along with iteration
-- count.
data PFStats =
  PFStats
  { pfIterations :: Int
  , pfBusFlows :: [BusFlow]
  , pfLineFlows :: [LineFlow]
  }

-- | Results for a bus from the power flow: its ID, its voltage and its power
-- inflow.
data BusFlow = BusFlow NodeID CVoltage CPower

-- | Results for the lines from a power flow.
data LineFlow = LineFlow EdgeID NodeID NodeID CPower CPower



-- All results:
-- | Solve and produce statistics for a power flow.
pfStatsSolve :: (Topological a, Bus b, Generator c, Load d
                , Line e, ShuntAdmittance f)
             => Method -- ^ Power flow method.
             ->  a  -- ^ System topology.
             -> [b] -- ^ System buses.
             -> [c] -- ^ System generators.
             -> [d] -- ^ System loads.
             -> [e] -- ^ System lines.
             -> [f] -- ^ System shunt admittances.
             -> PFStats -- ^ The power flow result.
pfStatsSolve m top bs gs los lis sads =
  pfStats top bs lis (solvePF m top bs gs los lis sads)

-- | Get the summary statistics for a power flow result.
pfStats :: (Topological a, Bus b, Line c)
        =>  a  -- ^ The grid topology.
        -> [b] -- ^ Grid buses.
        -> [c] -- ^ System lines.
        -> PFResult -- ^ A power flow result.
        -> PFStats
pfStats top bs ls (PFResult trm it v adm) =
  PFStats
  { pfIterations = it
  , pfBusFlows = busFlows trm bs adm v
  , pfLineFlows = lineFlows trm top ls adm v
  }




-- Bus results.
-- | Calculate power and voltage at each bus.
busFlows :: (Bus a)
         => IntMap Int -- ^ Bus ID changes made for PF solving.
         -> [a] -- ^ System buses.
         -> Matrix CAdmittance -- ^ Admittance matrix used for solution.
         -> Vector CVoltage -- ^ Result voltages from solution.
         -> [BusFlow]
busFlows trm bs adm v = fmap calcRes bs
  where
    -- Totally determined powers are:
    s = v * conj (adm #> v)
    -- The entry for each bus can be calculated separately, thus fmap.
    calcRes b = BusFlow (nodeID b) (v!i) (s!i)
      where
        i = (trm M.! nodeID b) - 1



-- Line details:
-- | Calculate line ID, ID's of the from/to bus, and from/to powers.
lineFlows :: (Topological a, Line b)
          => IntMap Int -- ^ Bus ID changes made for PF solving.
          ->  a  -- ^ System topology.
          -> [b] -- ^ System lines.
          -> Matrix CAdmittance -- ^ Admittance matrix.
          -> Vector CVoltage -- ^ Result voltage vector.
          -> [LineFlow]
lineFlows trm top ls adm v = fmap calcRes (joinEdges top ls)
  where
    -- The entry for each line can be calculated separately, thus fmap.
    calcRes (Edge n (iO,jO), _) = LineFlow n iO jO (pq i j) (pq j i)
      where
        i = (trm M.! iO) - 1
        j = (trm M.! jO) - 1
    -- The power through a line from i to j is given by this.
    -- Not very legible, but that's just the way it is.
    pq i j = --(v!(i-1))^(2::Int) * V.sum (adm!(i-1))
      - v!i * conjugate ((v!i - v!j) * adm!i!j)

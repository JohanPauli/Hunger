{- |
  Ordering and extracting statistics from a power flow solution.
-}
module Analysis.Statistics.PowerFlow
(
-- * Detail statistics
  PowerFlowResult
, BusResult
, LineResult
, busDetails
, lineDetails
)
where



-- IntMapping:
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

-- Local:
import Data.VecMat as V
import Data.Grid.Types
import Data.Grid.Topology
import Data.Grid.Node
import Data.Grid.Edge



-- | Power flow statistics are bus and line statistics, along with iteration
-- count.
type PowerFlowResult = (Int, [BusResult], [LineResult])

-- | Results for the buses from a power flow.
type BusResult = (NodeID, CVoltage, CPower)

-- | Calculate power and voltage at each bus.
busDetails :: (Bus a)
           => IntMap Int -- ^ Bus ID changes made for PF solving.
           -> [a] -- ^ System buses.
           -> Matrix CAdmittance -- ^ Admittance matrix used for solution.
           -> Vector CVoltage -- ^ Result voltages from solution.
           -> [BusResult]
busDetails trm bs adm v = fmap calcRes bs
  where
    -- Totally determined powers are:
    pows = v * conj (adm #> v)
    -- The entry for each bus can be calculated separately, thus fmap.
    calcRes b = (nodeID b, v!(i-1), pows!(i-1))
      where
        i = trm M.! nodeID b



-- Line details:
-- | Results for the lines from a power flow.
type LineResult = (EdgeID, NodeID, NodeID, CPower, CPower)

-- | Calculate line ID, ID's of the from/to bus, and from/to powers.
lineDetails :: (Topological a, Line b)
           => IntMap Int -- ^ Bus ID changes made for PF solving.
           ->  a  -- ^ System topology.
           -> [b] -- ^ System lines.
           -> Matrix CAdmittance -- ^ Admittance matrix.
           -> Vector CVoltage -- ^ Result voltage vector.
           -> [LineResult]
lineDetails trm top ls adm v = fmap calcRes (joinEdges top ls)
  where
    -- The entry for each line can be calculated separately, thus fmap.
    calcRes (_, Edge n (iO,jO)) = (n, iO, jO, pq i j, pq j i)
      where
        i = trm M.! iO
        j = trm M.! jO
    -- The power through a line from i to j is given by this.
    -- Not very legible, but that's just the way it is.
    pq i j = (v!(i-1))^(2::Int) * V.sum (adm!(i-1))
      - v!(i-1) * conjugate ((v!(i-1) - v!(j-1)) * adm!(i-1)!(j-1))

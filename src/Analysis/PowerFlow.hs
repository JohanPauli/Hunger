{- |
  General(ish) interface to power flow solvers.
-}
module Analysis.PowerFlow
(
-- * Methods
  Method (..)

-- * Solver interface
, solvePF

-- * Solver preparation.
, prepareAC
) where



-- Lists and sorting:
import Data.List (sort,group)

-- Fancy fmapping:
import Data.Functor ((<$>))

-- IntMapping:
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

-- Local:
import Data.VecMat (Matrix, Vector)
import Data.Grid.Types
import Data.Grid.Topology
import Data.Grid.Node
import Data.Grid.Edge
import Analysis.Data.Grid
import Analysis.Statistics.PowerFlow
import qualified Analysis.PowerFlow.Gauss as GS
import qualified Analysis.PowerFlow.Jacobi as JC



-- Methods:
-- | Known solution methods for power flows.
data Method =
    GaussSeidel -- ^ The Gauss-Seidel method from `Analysis.PowerFlow.Gauss`.
  | Jacobi -- ^ The Jacobi method from `Analysis.PowerFlow.Jacobi`.



-- | An AC power flow requires the knowning about system lines, shunt
-- admittances, topology, buses, generators and loads.
--
-- This may be too turn out to be too complicated of an interface (i.e. using
-- 6 arguments). Hopefully it's better than hiding data in product types.
solvePF :: ( Topological a, Bus b, Generator c
           , Load d, Line e, ShuntAdmittance f)
        => Method -- ^ Power flow method.
        ->  a  -- ^ System topology.
        -> [b] -- ^ System buses.
        -> [c] -- ^ System generators.
        -> [d] -- ^ System loads.
        -> [e] -- ^ System lines.
        -> [f] -- ^ System shunt admittances.
        -> PowerFlowStatistics -- ^ The power flow result.
solvePF GaussSeidel top bs gs los lis sads =
  ( it
  , busDetails tr bs adm v
  , lineDetails tr top lis adm v)
  where
    (_,v,it) = GS.solvePF nPQ nPV (s0,v0) adm
    (tr, nPQ, nPV, adm, s0, v0) =
      prepareAC top bs gs los lis sads
solvePF Jacobi top bs gs los lis sads =
  ( it
  , busDetails tr bs adm v
  , lineDetails tr top lis adm v)
  where
    (_,v,it) = JC.solvePF nPQ nPV (s0,v0) adm
    (tr, nPQ, nPV, adm, s0, v0) =
      prepareAC top bs gs los lis sads


-- | Prepare the power flow, classifying and sorting buses then building
-- the admittance matrix and initial power and voltage vectors.
prepareAC :: ( Topological a, Bus b, Generator c, Load d
             , Line e, ShuntAdmittance f )
          =>  a  -- ^ System topology.
          -> [b] -- ^ System buses.
          -> [c] -- ^ System generators.
          -> [d] -- ^ System loads.
          -> [e] -- ^ System lines.
          -> [f] -- ^ System shunt admittances.
          -> ( IntMap Int, Int, Int
             , Matrix CAdmittance, Vector CPower, Vector CVoltage)
prepareAC top bs gs los lis sads = (tr, nPQ, nPV, adm, s0, v0)
  where
    -- First, the buses are classified (gen => PV, biggest gen => Slack).
    classes = classifyBuses bs gs
    tr = M.fromList $ zip sorted [1..]
    sorted = (nodeID . escape) <$> sort classes
    -- Finally, the admittance matrix as well as inital power and voltage
    -- vectors are made.
    adm = admittanceMatrix tr top bs lis sads
    s0 = powerVec tr gs los
    v0 = voltageVec tr bs gs
    -- The number of PQ and PV buses.
    [nPQ,nPV,_] = (fmap length . group . sort) classes

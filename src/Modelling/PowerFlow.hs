{- |
  General(ish) interface to power flow solvers.
-}
module Modelling.PowerFlow
(
-- * Types
  Method (..)
, PFResult (..)

-- * Solver interface
, solvePF
) where



-- Lists and sorting:
import Data.List (sort,group)

-- IntMapping:
import Data.IntMap (IntMap)

-- Electrical types ,vectors, and matrices:
import Util.Types
import Util.Vector (Vector)
import Util.Matrix (Matrix)

-- Interface stuff:
import Interface.Topology
import Interface.Node
import Interface.Edge

-- Construction of power flow data structures:
import Conversion.Grid

-- Specific power flow algorithms:
import qualified Modelling.PowerFlow.Gauss as GS
import qualified Modelling.PowerFlow.Jacobi as JC
import qualified Modelling.PowerFlow.Newton as NR
import qualified Modelling.PowerFlow.NewtonJ as NJ



-- Types:
-- | Known solution methods for power flows.
data Method =
    GaussSeidel -- ^ The Gauss-Seidel method.
  | Jacobi -- ^ The Jacobi method.
  | NewtonRaphson -- ^ The Newton-Raphson method with approximated jacobian.
  | NewtonRaphsonJ -- ^ The Newton-Raphson method with analyic jacobian.

-- | A Power flow result.
data PFResult =
  PFResult
  { pfNMap :: IntMap Int -- ^ IntMap of shifts used by the solution algorithm.
  , pfIt :: Int -- ^ Number of iterations.
  , pfV :: Vector CVoltage -- ^ Result vector.
  , pfY :: Matrix CAdmittance -- ^ System admittance matrix.
  }



-- | A power flow requires the knowning about system lines, shunt
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
        -> PFResult -- ^ The power flow result.
solvePF method top bs gs los lis sads =
  case method of
    GaussSeidel -> uncurry (PFResult tr) (GS.solvePF nPQ nPV s0 v0 adm) adm
    Jacobi -> uncurry (PFResult tr) (JC.solvePF nPQ nPV s0 v0 adm) adm
    NewtonRaphson -> uncurry (PFResult tr) (NR.solvePF nPQ nPV s0 v0 adm) adm
    NewtonRaphsonJ -> uncurry (PFResult tr) (NJ.solvePF nPQ nPV s0 v0 adm) adm
  where
    -- Classify buses as PQ, PV, and Slack.
    classes = classifyBuses bs gs
    -- Sort the bus IDs for use with algorithms.
    tr = nodeMap classes
    -- Initial values for power, voltage, and the admittance matrix.
    s0 = makeS tr gs los
    v0 = makeV tr bs gs
    adm = makeY tr top lis sads
    -- Number of buses in each class.
    [nPQ,nPV,_] = (fmap length . group . sort) classes

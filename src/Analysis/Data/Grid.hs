{- |
  Translations on power systems.

  This is mostly for generating data structures suitable for use
  in mathematical models.
-}
module Analysis.Data.Grid
(
-- * Bus classification
  PFType (..)
, escape
, classifyBuses

-- * Power flow data structure construction
, powerVec
, voltageVec
, admittanceMatrix
) where



-- List processing:
import Data.List (sortBy)
import Data.Function (on)

-- Folding:
import qualified Data.Foldable as F

-- Matrices/Vectors:
import Data.VecMat (Matrix, Vector, (!))
import qualified Data.VecMat as V

-- IntMaps:
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

-- Local:
import Data.Grid.Types
import Data.Grid.Topology
import Data.Grid.Node
import Data.Grid.Edge



-- | The three types of buses found in a power flow analysis.
--
-- Acts as a context in which power flow analysis is done.
data PFType a =
    PQ a -- ^ PQ element, usually a bus without generators.
  | PV a -- ^ PV element, usually a bus with generators.
  | SL a -- ^ Slack element, usually the bus with the largest generator.
  deriving (Show)

-- | The obvious functor implementation.
instance Functor PFType where
  fmap f (PQ a) = PQ (f a)
  fmap f (PV a) = PV (f a)
  fmap f (SL a) = SL (f a)

-- | These things are equal if the label is equal.
-- (There's probably a better way to do this.)
instance Eq (PFType a) where
  (PQ _)==(PQ _) = True
  (PV _)==(PV _) = True
  (SL _)==(SL _) = True
  _==_ = False

-- | These things are orderd as PQ<PV<SL.
-- (There's probably a better way to do this.)
instance Ord (PFType a) where
  compare (PQ _) (PV _) = LT
  compare (PQ _) (SL _) = LT
  compare (PV _) (SL _) = LT
  compare (PV _) (PV _) = EQ
  compare (PQ _) (PQ _) = EQ
  compare (SL _) (SL _) = EQ
  compare _ _ = GT

-- | If something with a PFType is noded, then PFType version is too.
instance (Noded a) => Noded (PFType a) where
  nodeID = nodeID . escape
  setNodeID i = fmap (setNodeID i)

-- | Remove the context from a `PFType` element.
escape :: PFType a -> a
escape (PQ a) = a
escape (PV a) = a
escape (SL a) = a



-- Enumerate the PQ and PV buses:
-- | Classify buses as `PQ`, `PV`, and `SL` after a simple heuristic.
--
-- Buses without generators are `PQ` buses, buses with generators are
-- `PV` generators, and the (single) bus with the generator with most
-- available capacity is the slack (`SL`) bus.
classifyBuses :: (Bus a, Generator b)
          => [a]
          -> [b]
          -> [PFType a]
classifyBuses bs gs = fmap label bs
  where
    label b
      | nodeID b==slID = SL b
      | M.member (nodeID b) genMap = PV b
      | otherwise = PQ b
    -- A map for checking if any generator is at any bus.
    genMap = M.fromList [(nodeID g,True) | g <- gs]
    -- The slack bus ID is the ID of the bus with the generator with
    -- the largest available generation.
    slID = (nodeID . head) gs'
    gs' = sortBy (compare `on` (\x -> realPart $ genPower x - genMax x)) gs



-- Power vector construction.
-- | Construct the 'Sbus' vector, or the vector of all known power values.
--
-- Only values from generators and loads are known in this case.
--
--  Assumes that values are in per-unit format.
powerVec :: (Generator a, Load b)
     => IntMap Int -- ^ Bus ID transformations.
     -> [a] -- ^ Generators.
     -> [b] -- ^ Loads.
     -> Vector CPower -- ^ Result, a power vector.
powerVec tr gens loads =
    (\v -> foldl addGen v gens)
  $ foldl addLoad (V.replicate len (0:+0)) loads
  where
    -- The number of entries in the vector should be the same as the number
    -- of nodes in the topology (assumes contiguously numbered nodes, may
    -- not be true and everything is ruined; the program will likely crash).
    len = M.size tr

    -- Add a generator's power to the vector.
    addGen v gen = V.updateV i (v!(i-1) + genPower gen) v
      where
        i = tr M.! nodeID gen

    -- Subtract
    addLoad v load = V.updateV i (v!(i-1) - loadPower load) v
      where
        i = tr M.! nodeID load



-- Voltage vector construction.
-- | Construct the V0 vector, or the vector of all known voltages.
-- Constructs a vector with every PV voltage and the Slack voltage, then
-- assumes every other value to be 1:+0.
--
-- Assumes that values are in per-unit format.
voltageVec :: (Bus a, Generator b)
           => IntMap Int -- ^ Node ID transformations.
           -> [a] -- ^ All buses, slack in the back.
           -> [b] -- ^ All generators.
           -> Vector CVoltage -- ^ Result, a voltage vector.
voltageVec tr buses gs =
    (`addBus` sl)
  $ (\x -> foldl addGen x gs)
  $ foldl addBus (V.replicate len (1:+0)) bs
  where
    (sl:bs) = reverse buses
    len = length buses
    addBus v b = V.updateV (tr M.! nodeID b) (busVoltage b) v
    addGen v g = V.updateV i (newV:+imagV) v
      where
        i = tr M.! nodeID g
        imagV = imagPart $ v!(i-1)
        newV = genVoltage g / magnitude ((v!(i-1))^(2::Int))



-- Admittance matrices.
-- | Make the Y-bus matrix. Assumes that values are in per-unit format.
admittanceMatrix :: (Topological a, Bus b, Line c, ShuntAdmittance d)
                 => IntMap Int -- ^ Node ID transformations.
                 ->  a  -- ^ System topology.
                 -> [b] -- ^ System buses (implies dimension of matrix).
                 -> [c] -- ^ Lines (simple, impedances).
                 -> [d] -- ^ Shunt admittances (bus-connected components).
                 -> Matrix CAdmittance -- ^ Result, an admittance matrix.
admittanceMatrix tr top bs ls as =
    flip (F.foldl' addSA) as
  $ flip (F.foldl' addLine) joined
  $ V.diag (V.replicate len 0)
  where
    -- Dimension of the admittance matrix is the number of buses in the grid.
    len = length bs
    -- Lines must know their position in the grid.
    joined = joinEdges top ls

    -- Add a shunt admittance to the admittance matrix.
    -- For an admittance at i, adds a new admittance to the ii entry.
    addSA :: (ShuntAdmittance a)
          => Matrix CAdmittance -> a -> Matrix CAdmittance
    addSA m a = V.updateM (i,i) admNew m
      where
        i = tr M.! nodeID a
        admNew = shuntAdmittance a + m!(i-1)!(i-1)

    -- Add a line admittance to the admittance matrix.
    -- For a line from i to j, it adds a new admittance to the ij, ji
    -- ii, and jj entries of the matrix.
    addLine :: (Line a)
            => Matrix CAdmittance -> (a,Edge) -> Matrix CAdmittance
    addLine m (l, Edge _ (iO,jO)) =
        V.updateM (i,j) (-admNew + m!(i-1)!(j-1))
      $ V.updateM (j,i) (-admNew + m!(j-1)!(i-1))
      $ V.updateM (j,j) (admNew + susc + m!(j-1)!(j-1))
      $ V.updateM (i,i) (admNew + susc + m!(i-1)!(i-1)) m
      where
        i = tr M.! iO
        j = tr M.! jO
        susc = 0 :+ lineB l / 2
        admNew = 1 / lineZ l

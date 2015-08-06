{- |
  Natural data representation similar to the MatPower format.

  The grid type is a database-like structure, while the bus, generator,
  and line types are record-like structures.
-}
module Natural.Simple
(
-- * Database-Like Grid
  SGrid (..)
, emptySGrid

-- * Record-Like Bus
, SBus (..)

-- * Record-Like Generator
, SGen (..)

-- * Record-Like Line
, SLine (..)
) where



-- Electrical types:
import Util.Types

-- (Interfaces to) Topological, grid, and device information:
import Interface.Topology
  (Topological (..), Noded (..), Edged (..)
  , NodeID, EdgeID, Node (..), Edge (..) )
import Interface.Grid (PerUnit (..))
import Interface.Node (Bus (..), Generator (..), Load (..), ShuntAdmittance (..))
import Interface.Edge



-- Database-like grid:
-- | A grid type sufficient for Power Flow analysis and little else; for
-- demonstration purposes.
data SGrid =
  SGrid
  { gridName :: String -- ^ Grid name (e.g. geographical location name).
  , gridMVAbase :: Power -- ^ Grid MVA base, for p.u. conversion.
  , gridBuses :: Buses -- ^ The buses of the grid.
  , gridGens :: Gens -- ^ Generators in the grid.
  , gridLines :: Lines
  } deriving (Show)

-- | This type of grid carries topological information.
--
-- The grid topology can be derived from buses and lines and their relation.
instance Topological SGrid where
  nodes = fmap (Node . nodeID) . gridBuses
  edges = fmap (\l -> Edge (edgeID l) (slineFrom l, slineTo l)) . gridLines

-- | The grid carries values which are not in per-unit format, and as such
-- must be normalized for use.
instance PerUnit SGrid where
  normalize grid@SGrid{gridMVAbase=baseMVA, gridBuses=bs, gridGens=gs} =
    grid
    { gridBuses = fmap (\b -> b{sbusPower=sbusPower b/base
                               ,sbusAdmittance=sbusAdmittance b/base}) bs
    , gridGens = fmap (\g -> g{sgenPower=sgenPower g/base
                              ,sgenMax=sgenMax g/base
                              ,sgenMin=sgenMin g/base}) gs }
    where
      base = baseMVA :+ 0
  denormalize grid@SGrid{gridMVAbase=baseMVA, gridBuses=bs, gridGens=gs} =
    grid
    { gridBuses = fmap (\b -> b{sbusPower=sbusPower b*base
                               ,sbusAdmittance=sbusAdmittance b*base}) bs
    , gridGens = fmap (\g -> g{sgenPower=sgenPower g*base
                              ,sgenMax=sgenMax g*base
                              ,sgenMin=sgenMin g*base}) gs }
    where
      base = baseMVA :+ 0

-- | An empty grid, for parsing into
emptySGrid :: SGrid
emptySGrid = SGrid "" 0.0 [] [] []



-- Buses:
-- | Synonym for several lines in a list.
type Buses = [SBus]

-- | A simple bus type with additional information about shunt admittance and
-- connected loads.
data SBus =
  SBus
  { sbusName :: String -- ^ The bus name (e.g. geographical location name).
  , sbusID :: NodeID -- ^ The bus number.
  , sbusVoltage :: CVoltage -- ^ Bus actual voltage (p.u.).
  , sbusVoltageBase :: Voltage -- ^ Bus nominal voltage (e.g. 60 kV).
  , sbusAdmittance :: CAdmittance -- ^ Total shunt admittance at bus.
  , sbusPower :: CPower -- ^ Total load at this bus.
  } deriving (Show)

-- | Buses are node-connected.
instance Noded SBus where
  nodeID = sbusID
  setNodeID i b = b{sbusID=i}

-- | Buses are buses, obviously.
instance Bus SBus where
  busVoltage = sbusVoltage

-- | These buses also carry load information.
instance Load SBus where
  loadS = sbusPower

-- | As well as shunt admittance information.
instance ShuntAdmittance SBus where
  shuntY = sbusAdmittance



-- Generators:
-- | Synonym for several generators in a list.
type Gens = [SGen]

-- | A simple generator type.
data SGen =
  SGen
  { sgenBus :: NodeID -- ^ The bus at which the generator resides.
  , sgenPower :: CPower -- ^ Current generation at generator.
  , sgenMax :: CPower -- ^ Maximum generating capacity.
  , sgenMin :: CPower -- ^ Minimum generating capacity.
  , sgenVoltage :: Voltage -- ^ Voltage magnitude setpoint.
  } deriving (Show)

-- | Generators are at nodes.
instance Noded SGen where
  nodeID = sgenBus
  setNodeID i g = g{sgenBus=i}

-- | Generators are generators.
instance Generator SGen where
  genS = sgenPower
  genMax = sgenMax
  genMin = sgenMin
  genVm = sgenVoltage



-- Lines:
-- | Synonym for several lines in a list.
type Lines = [SLine]

-- | A simple line type.
data SLine =
  SLine
  { slineID :: EdgeID -- ^ The line number.
  , slineFrom :: NodeID -- ^ Line 'origin' bus.
  , slineTo :: NodeID -- ^ Line 'destination' bus.
  , slineImpedance :: CImpedance -- ^ Line impedance.
  , slineSusceptance :: Susceptance -- ^ Line charging susceptance.
  } deriving (Show)

-- | Lines are edges in the topology, and so are connected to edges.
instance Edged SLine where
  edgeID = slineID

-- | The line is a line, obviously.
instance Line SLine where
  lineZ = slineImpedance
  lineB = slineSusceptance

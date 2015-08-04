{- |
  An implementation of a natural grid representation.

  This is somewhat like the MatPower format for grids.
-}
module Data.Grid.Simple
(
-- * Database-like Grid
  Grid (..)
, emptyGrid

-- * Record-like Bus
, Buses
, SBus (..)

-- * Record-like Generator
, Gens
, SGen (..)

-- * Record-like Line
, Lines
, SLine (..)
) where



-- Local:
import Data.Grid.Types
import Data.Grid.Topology
import Data.Grid.Node
import Data.Grid.Edge



-- Database-like grid:
-- | A grid type sufficient for Power Flow analysis and little else; for
-- demonstration purposes.
data Grid =
  Grid
  { gridName :: Name -- ^ Grid name (e.g. geographical location name).
  , gridMVAbase :: Power -- ^ Grid MVA base, for p.u. conversion.
  , gridBuses :: Buses -- ^ The buses of the grid.
  , gridGens :: Gens -- ^ Generators in the grid.
  , gridLines :: Lines
  } deriving (Show)

-- | This type of grid carries topological information.
--
-- The grid topology can be derived from buses and lines and their relation.
instance Topological Grid where
  nodes = fmap (Node . nodeID) . gridBuses
  edges = fmap (\l -> Edge (edgeID l) (slineFrom l, slineTo l)) . gridLines

-- | The grid carries values which are not in per-unit format, and as such
-- must be normalized for use.
instance PerUnit Grid where
  normalize grid@Grid{gridMVAbase=baseMVA, gridBuses=bs, gridGens=gs} =
    grid
    { gridBuses = fmap (\b -> b{sbusPower=sbusPower b/base
                               ,sbusAdmittance=sbusAdmittance b/base}) bs
    , gridGens = fmap (\g -> g{sgenPower=sgenPower g/base
                              ,sgenMax=sgenMax g/base
                              ,sgenMin=sgenMin g/base}) gs }
    where
      base = baseMVA :+ 0
  denormalize grid@Grid{gridMVAbase=baseMVA, gridBuses=bs, gridGens=gs} =
    grid
    { gridBuses = fmap (\b -> b{sbusPower=sbusPower b*base
                               ,sbusAdmittance=sbusAdmittance b*base}) bs
    , gridGens = fmap (\g -> g{sgenPower=sgenPower g*base
                              ,sgenMax=sgenMax g*base
                              ,sgenMin=sgenMin g*base}) gs }
    where
      base = baseMVA :+ 0

-- | An empty grid, for parsing into
emptyGrid :: Grid
emptyGrid = Grid "" 0.0 [] [] []



-- Buses:
-- | Synonym for several lines in a list.
type Buses = [SBus]

-- | A simple bus type with additional information about shunt admittance and
-- connected loads.
data SBus =
  SBus
  { sbusName :: Name -- ^ The bus name (e.g. geographical location name).
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
  loadPower = sbusPower

-- | As well as shunt admittance information.
instance ShuntAdmittance SBus where
  shuntAdmittance = sbusAdmittance



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
  genPower = sgenPower
  genMax = sgenMax
  genMin = sgenMin
  genVoltage = sgenVoltage



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

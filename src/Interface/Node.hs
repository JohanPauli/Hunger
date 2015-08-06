{- |
  Properties of node-connected components.
-}
module Interface.Node
(
-- * Basic node characteristics
  Bus (..)

-- * Static node-connected components
, ShuntAdmittance (..)
, Generator (..)
, Load (..)
) where



-- Electrical types:
import Util.Types

-- (Interfaces to) Topological information:
import Interface.Topology



-- Properties having to do with buses.
-- | A bus something which is connected to a topology node, and which has
-- a uniform voltage level.
class (Noded a) => Bus a where
  busVoltage :: a -> CVoltage
  busVoltage b = mkPolar (busVMagnitude b) (busVAngle b)
  busVMagnitude :: a -> Voltage
  busVMagnitude = magnitude . busVoltage
  busVAngle :: a -> Angle
  busVAngle = phase . busVoltage


-- | A Shunt admittance.
--
-- Minimal complete definition: `shuntY` or (`shuntG` and `shuntB`).
class (Noded a) => ShuntAdmittance a where
  shuntY :: a -> CAdmittance
  shuntY a = shuntG a :+ shuntB a
  shuntG :: a -> Conductance
  shuntG = realPart . shuntY
  shuntB :: a -> Susceptance
  shuntB = imagPart . shuntY

-- | A generator, information about generation at a node.
--
-- Minimal complete definiton: `genVm`, `genMin`, `genMax`, and (`genS` or
-- (`genP` and `genQ`)).
class (Noded a) => Generator a where
  genS :: a -> CPower
  genS g = genP g :+ genQ g
  genP :: a -> Power
  genP = realPart . genS
  genQ :: a -> Power
  genQ = imagPart . genS
  genMax :: a -> CPower
  genMin :: a -> CPower
  genVm :: a -> Voltage

-- | A Load, information about demand at a node.
--
-- Minimal complete definition: `loadS` or (`loadP` and `loadQ`).
class (Noded a) => Load a where
  loadS :: a -> CPower
  loadS l = loadP l :+ loadQ l
  loadP :: a -> Power
  loadP = realPart . loadS
  loadQ :: a -> Power
  loadQ = imagPart . loadS

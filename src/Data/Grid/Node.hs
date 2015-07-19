{- |
  Properties of node-connected components.
-}
module Data.Grid.Node
(
-- * Everything here is node-connected
  Noded (..)

-- * Basic node characteristics
, Bus (..)

-- * Static node-connected components
, ShuntAdmittance (..)
, Generator (..)
, Load (..)
) where



-- Local:
import Data.Grid.Types
import Data.Grid.Topology



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
class (Noded a) => ShuntAdmittance a where
  shuntAdmittance :: a -> Complex Admittance
  shuntAdmittance a = shuntConductance a :+ shuntReactance a
  shuntConductance :: a -> Conductance
  shuntConductance = realPart . shuntAdmittance
  shuntReactance :: a -> Reactance
  shuntReactance = imagPart . shuntAdmittance

-- | A generator, information about generation at a node.
class (Noded a) => Generator a where
  genPower :: a -> CPower
  genMax :: a -> CPower
  genMin :: a -> CPower
  genVoltage :: a -> Voltage

-- | A Load, information about demand at a node.
class (Noded a) => Load a where
  loadPower :: a -> CPower

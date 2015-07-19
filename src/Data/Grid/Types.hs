{- |
  Commonly used types for the system.
-}
module Data.Grid.Types
(
-- * Normalizable units.
  PerUnit (..)

-- * Basic types
, Name

-- * Electrical types
, Power, CPower
, Voltage, CVoltage
, Current, CCurrent
, Impedance, CImpedance
, Resistance, Reactance
, Admittance, CAdmittance
, Conductance, Susceptance

-- * Angles and rad-deg conversions
, Angle
, toRad
, toDeg

-- * Other libraries
, module Data.Complex
)
where



-- Complex numbers:
import Data.Complex



-- | Things which are normalizable to per unit.
--
-- Obviously, @denormalize . normalize = id@.
class PerUnit a where
  normalize :: a -> a
  denormalize :: a -> a



-- Basic types.
-- | Names for things.
type Name = String



-- Electrical types.
-- | Power.
type Power = Double

-- | Complex power.
type CPower = Complex Double

-- | Voltage.
type Voltage = Double

-- | Complex voltage.
type CVoltage = Complex Double

-- | Current.
type Current = Double

-- | Complex current.
type CCurrent = Complex Double

-- | Impedance.
type Impedance = Double

-- | Complex impedance.
type CImpedance = Complex Double

-- | Resistance.
type Resistance = Double

-- | Reactance.
type Reactance = Double

-- | Admittance.
type Admittance = Double

-- | Complex admittance.
type CAdmittance = Complex Double

-- | Conductance.
type Conductance = Double

-- | Susceptance.
type Susceptance = Double



-- Angles
-- | Conversion from degrees to radians.
type Angle = Double

-- | Convert from degrees to radians.
toRad :: (Floating a) => a -> a
toRad = (*) (pi/180.0)

-- | Convert from radians to degrees.
toDeg :: (Floating a) => a -> a
toDeg = (*) (180.0/pi)

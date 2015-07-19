{- |
  Properties of edge-connected components.
-}
module Data.Grid.Edge
(
-- * Basic kinds of lines
  Line (..)
, LongLine (..)
) where



-- Local:
import Data.Grid.Types
import Data.Grid.Topology



-- Properties having to do with lines.
-- | A Line is anything `EdgeConnected` which has resistance and reactance.
class (Edged a) => Line a where
  lineZ :: a -> CImpedance -- ^ Line impedance (p.u.).
  lineZ l = lineR l :+ lineX l
  lineR :: a -> Resistance -- ^ Line resistance (p.u.).
  lineR = realPart . lineZ
  lineX :: a -> Reactance -- ^ Line reactance (p.u.).
  lineX = imagPart . lineZ

-- | A long line is a `Line` which must account for charging susceptance.
-- Long lines are lines that are more than about 200 km long.
class (Line a) => LongLine a where
  llSusceptance :: a -> Susceptance -- ^ Line charging susceptance.

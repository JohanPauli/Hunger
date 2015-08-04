{- |
  Properties of edge-connected components.
-}
module Data.Grid.Edge
(
-- * Basic kinds of lines
  Line (..)
) where



-- Local:
import Data.Grid.Types
import Data.Grid.Topology



-- Properties having to do with lines.
-- | A Line is anything `EdgeConnected` which has resistance, reactance, and
-- a charging susceptance.
class (Edged a) => Line a where
  lineZ :: a -> CImpedance -- ^ Line impedance (p.u.).
  lineZ l = lineR l :+ lineX l
  lineR :: a -> Resistance -- ^ Line resistance (p.u.).
  lineR = realPart . lineZ
  lineX :: a -> Reactance -- ^ Line reactance (p.u.).
  lineX = imagPart . lineZ
  lineB :: a -> Susceptance

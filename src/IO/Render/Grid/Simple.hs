{- |
  Facilities for rendering power system data to text.
-}
module IO.Render.Grid.Simple
(
-- * SGrid rendering
  renderSGrid

-- * Line rendering
, renderLine
, renderLines

-- * Bus rendering
, renderBus
, renderBuses
) where



-- Rendering utilities:
import IO.Render.Util

-- The rendered data structure:
import Natural.Simple



-- SGrid rendering.
-- | Render a `SGrid` to a file, in rigid format.
renderSGrid :: Renderer SGrid
renderSGrid
  SGrid
  { gridName=name
  , gridMVAbase=base
  , gridBuses=bs
  , gridGens=gs
  , gridLines=ls
  } =
     "SGrid name:\n"
  <> renderString name <> endl
  <> endl
  <> "SGrid power base [MVA] (for p.u. conversion):" <> endl
  <> toBS base <> endl
  <> endl
  <> "SGrid buses" <> endl
  <> "Name" <> sep <> "ID" <> sep <> "Load [p.u.]" <> sep
  <> "Admittance [p.u.]" <> sep <> "Voltage [p.u.]" <> sep
  <> "Vbase [kV]" <> endl
  <> renderBuses bs
  <> endl
  <> "SGrid generators" <> endl
  <> "Bus ID" <> sep <> "Generation [p.u.]" <> endl
  <> renderGens gs
  <> endl
  <> "SGrid lines" <> endl
  <> "ID" <> sep <> "fBus" <> sep <> "tBus" <> sep
  <> "lImp [p.u.]" <> sep <> "B [p.u.]" <> endl
  <> renderLines ls



-- Bus rendering.
-- | Render `Buses` as text.
renderBuses :: Renderer [SBus]
renderBuses = foldMap $ \bus -> renderBus bus <> endl

-- | Render the information contained in a `SBus`.
renderBus :: Renderer SBus
renderBus
  SBus
  { sbusID=bID
  , sbusName=name
  , sbusPower=bPow
  , sbusAdmittance=bAdm
  , sbusVoltage=bV
  , sbusVoltageBase=bVb
  } =
     renderString name
  <> sep
  <> toBS bID
  <> sep
  <> renderComplex bPow
  <> sep
  <> renderComplex bAdm
  <> sep
  <> renderComplex bV
  <> sep
  <> toBS bVb



-- Generator rendering:
-- | Render 'Gens' as text.
renderGens :: Renderer [SGen]
renderGens = foldMap $ \gen -> renderGen gen <> endl

-- | Render the information contained in a `SGen`.
renderGen :: Renderer SGen
renderGen
  SGen
  { sgenBus=bID
  , sgenPower=pq
  } =
     toBS bID
  <> sep
  <> renderComplex pq



-- Line rendering.
-- | Render `Lines` as text.
renderLines :: Renderer [SLine]
renderLines = foldMap $ \line -> renderLine line <> endl

-- | Render the information contained in a `SLine`.
renderLine :: Renderer SLine
renderLine
  SLine
  { slineID=lID
  , slineFrom=lFrom
  , slineTo=lTo
  , slineImpedance=lImp
  , slineSusceptance=lSus
  } =
     toBS lID
  <> sep
  <> toBS lFrom
  <> sep
  <> toBS lTo
  <> sep
  <> renderComplex lImp
  <> sep
  <> toBS lSus

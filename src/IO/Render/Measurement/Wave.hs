{- |
  A renderer for wave measurements.
-}
module IO.Render.Measurement.Wave
(
-- * Wave data rendering
  renderWave
) where



-- Dates:
import Util.Time

-- Vectors:
import Util.Vector ((!))
import qualified Util.Vector as V

-- Rendering utilities:
import IO.Render.Util

-- The type to be rendered:
import Natural.Measurement.Wave



-- Rendering Wave data.
-- | Render a set of `Wave` measurements.
renderWave :: Renderer WaveData
renderWave (WaveData times hs ts) =
     "Date" <> sep <> "Height[m]" <> sep <> "Period[s]"
  <> endl
  <> V.foldl' addEntry "" (V.enumFromN 0 $ V.length times - 1)
  where
    addEntry str i = str <> renderWaveEntry (times!i,hs!i,ts!i) <> endl

-- | Render a single entry in a `Wave` data structure.
renderWaveEntry :: Renderer (Int, Double, Double)
renderWaveEntry (date,height,period) =
     pack (formatTime defaultTimeLocale "%F %T" (posixSecondsToUTCTime date))
  <> sep
  <> toBS height
  <> sep
  <> toBS period

{- |
  Renderer for SEV measurement data. Mostly for testing that parsing is
  done correctly.
-}
module IO.Render.Measurement.SEV
(
-- * Rendering data from SEV
  renderSEV
) where



-- Vectors, matrices and dates:
import Util.Vector (Vector)
import qualified Util.Vector as V
import Util.Matrix (Matrix, (!))
import Util.Time

-- Maps:
import Data.Map (Map)
import qualified Data.Map as I

-- Rendering utilities:
import IO.Render.Util

-- The rendered data type:
import Natural.Measurement.SEV



-- | Render a SEVData instance.
renderSEV :: Renderer SEVData
renderSEV (SEVData d t h) =
     renderHeader h <> endl
  <> renderData (t,d)

-- | Render the header section.
renderHeader :: Renderer (Map String Int)
renderHeader = toBS . I.toList

-- | Render the data section.
renderData :: Renderer (Vector Int, Matrix Double)
renderData (ts,vs) =
  V.foldl' makeEntry "" (V.enumFromN 0 $ V.length ts - 1)
  where
    makeEntry str i = str <> renderSEVEntry (ts V.! i,vs!i) <> endl


-- | Render a single entry of the data section.
renderSEVEntry :: Renderer (Int, Vector Double)
renderSEVEntry (date, v) =
     pack (formatTime defaultTimeLocale "%F %T" $ posixSecondsToUTCTime date)
  <> V.foldl' (\str x -> str <> sep <> toBS x) "" v

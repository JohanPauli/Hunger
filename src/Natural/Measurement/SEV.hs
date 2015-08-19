{- |
  Container for data format used by SEV.
-}
module Natural.Measurement.SEV where



-- Mapping (not IntMap because String-indexed):
import Data.Map (Map)

-- Vectors and matrices:
import Util.Vector (Vector)
import Util.Matrix (Matrix)



-- | The SEV data may be represented as a number of different measurement
-- columns
data SEVData =
  SEVData
  { sevData :: Matrix Double -- ^ Matrix of the measurement data.
  , sevTimes :: Vector Int -- ^ Measurement POSIX timestamps.
  , sevHeaders :: Map String Int -- ^ Locations of named attributes in matrix.
  }

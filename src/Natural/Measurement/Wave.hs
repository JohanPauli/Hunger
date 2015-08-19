{- |
  Container for Landsverk wave data.
-}
module Natural.Measurement.Wave where



-- Vectors:
import Util.Vector (Vector)



data WaveData =
  WaveData
  { waveTime :: Vector Int -- ^ POSIX timestamps.
  , waveH :: Vector Double -- ^ Wave heights.
  , waveT :: Vector Double -- ^ Wave energy periods.
  }

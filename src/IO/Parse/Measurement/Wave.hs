{- |
  Parser for wave measurements from Landsverk.
-}
module IO.Parse.Measurement.Wave
(
-- * Wave parser
  parseWave
) where



-- Time:
import Util.Time

-- Vectors:
import qualified Util.Vector as V

-- Parsing:
import IO.Parse.Util as P8

-- The parsed type:
import Natural.Measurement.Wave



-- Wave parser.
-- | Parse a file containing wave measurements.
parseWave :: Parser WaveData
parseWave =
     skipLine
  *> fmap mkWave (many parseWaveEntry)
  <* skipLine
  where
    mkWave = construct . vectorify . unzip3
    vectorify (l1,l2,l3) = (V.fromList l1, V.fromList l2, V.fromList l3)
    construct (v1,v2,v3) = WaveData v1 v2 v3



-- Parse entry.
-- | Parse an entry in the wave file.
parseWaveEntry :: Parser (Int, Double, Double)
parseWaveEntry = do
  date <- utcTimeToPOSIXTimestamp
    <$> parseTime "%Y %m %d %H %M %S"
    <$> P8.unpack <$> P8.take 16
  skipSpace
  height <- double
  skipSpace
  period <- double
  return (date, height, period)

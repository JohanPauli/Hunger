{- |
  The tidal model for the Faroe Islands developed at  the University of the
  Faroe Islands by Knud Simonsen and Bárður Niclasen.
  This has built-in data for three locations:
    * vest: Vestmannasund
    * leir: Leirvíksfjørður
    * hest: Hestfjørður
  The model calculates absolute tidal speed based on the number of
  hours since 2000-01-01 00:00:00.
  This was ported directly from a MATLAB script, hence ugliness.
-}

module Modelling.Tidal
( simulate
, vest
) where



-- Complex numbers:
import Data.Complex

-- Vectors and matrices:
import Util.Vector
import qualified Util.Vector as V
import Util.Matrix (scalar)

-- Tidal data:
import Modelling.Tidal.TidalData



-- | Simulate the tide during an interval of time.
simulate :: Vector Double -- ^ Vector of hours after the year 2000 to simulate.
         -> TidalConstants -- ^ Constants for the location to simulate.
         -> Vector Double -- ^ Result, in m/s.
simulate vec tc =
  V.map f vec
    where
      majs = getConst aMaj tc
      mins = getConst aMin tc
      phas = getConst fPha tc
      incs = getConst fInc tc
      corPhas = phas - phaCors + userPhaCors
      periods = (2.0 * pi) * freqs
      wp = 0.5 * (majs + mins)
      wm = majs - wp
      fp = incs - corPhas
      fpDeg = fp * (pi / 180.0)
      fm = incs + corPhas
      fmDeg = fm * (pi / 180.0)
      wpVal t = wp * V.map exp (scalar (0:+1.0)
        * (periods * scalar (t:+0) * fpDeg))
      wmVal t = wm * V.map exp (scalar (0:+(-1.0))
        * (periods * scalar (t:+0) - fmDeg))
      wSum t = V.sum $ wpVal t + wmVal t
      f :: Double -> Double
      f t = magnitude $ wSum t + res tc

{- |
  Hard-coded data for the tidal model.
  This is part of the port of a part of the tidal model created by
  Knud Simonsen and Bárður Niclasen.
-}
module Modelling.Tidal.TidalData
( Constituent
, TidalConstants
, aMaj, aMin, fPha, fInc, res
, freqs
, phaCors
, userPhaCors
, vest
, getConst
) where



-- Complex numbers:
import Data.Complex

-- Matrices and vectors:
import Util.Vector (Vector)
import qualified Util.Vector as V
import Util.Matrix ()



-- | Constituent oscillation description.
data Constituent =
  Constituent
  { constName :: String
  , aMaj :: Complex Double
  , aMin :: Complex Double
  , fPha :: Complex Double
  , fInc :: Complex Double }

-- | Tidal constants for a single place.
data TidalConstants =
  TidalConstants
  { name :: String
  , x :: Int
  , y :: Int
  , res :: Complex Double
  , consts :: [Constituent]
  }

-- | Frequencies of constituents.
fM2, fS2, fN2, fM4, fMS4, fO1 :: Complex Double
fM2 = 0.0805114007000000;
fS2  = 0.0833333333000000;
fN2  = 0.0789992488000000;
fM4  = 0.161022801300000;
fMS4 = 0.163844734000000;
fO1  = 0.0387306544000000;

freqs :: Vector (Complex Double)
freqs = V.fromList [fM2, fS2, fN2, fM4, fMS4, fO1]

-- | Phase corrections of constituents.
phaCorM2, phaCorS2, phaCorN2, phaCorM4, phaCorMS4, phaCorO1 :: Complex Double
phaCorM2  = 136.4902;
phaCorS2  = 8.8685e-07;
phaCorN2  = 8.0598;
phaCorM4  = 272.9804;
phaCorMS4 = 136.4902;
phaCorO1  = 126.5171;

phaCors :: Vector (Complex Double)
phaCors = V.fromList [phaCorM2, phaCorS2, phaCorN2
                     ,phaCorM4, phaCorMS4, phaCorO1]

userPhaCors :: Vector (Complex Double)
userPhaCors = V.fromList [12, 23, 16, 0, 0, -4]

-- | Tidal constants for Vestmannasund.
vest :: TidalConstants
vest = TidalConstants
  { name = "Vestmannasund"
  , x = 451
  , y = 1014
  , res = (-0.011046901671092):+(-0.018268443364031)
  , consts =
  [ Constituent
    { constName = "M2"
    , aMaj = 2.056402886505574
    , aMin = 9.735909199728443e-04
    , fPha = 1.483723839032930e+02
    , fInc = 1.233508450604320e+02 }
  , Constituent
    { constName = "S2"
    , aMaj = 0.611809103873135
    , aMin = 4.610430144283417e-04
    , fPha = 1.833976893554818e+02
    , fInc = 1.234226428148368e+02 }
  , Constituent
    { constName = "N2"
    , aMaj = 0.415693058297143
    , aMin = 4.346561189516596e-05
    , fPha = 1.188614768687388e+02
    , fInc = 1.233834839982897e+02 }
  , Constituent
    { constName = "M4"
    , aMaj = 0.031550947209604
    , aMin = 0.011812559185833
    , fPha = 3.507957452531133e+02
    , fInc = 1.265659110384279e+02 }
  , Constituent
    { constName = "MS4"
    , aMaj = 0.023287399213700
    , aMin = 0.007704875416046
    , fPha = 1.301593768714871e+02
    , fInc = 1.236175932048681e+02 }
  , Constituent
    { constName = "O1"
    , aMaj = 0.139066190349070
    , aMin = -0.001044745164691
    , fPha = 1.557125572131494e+02
    , fInc = 1.238064083212691e+02 } ] }

-- | Extract a constituent field from
getConst :: (Constituent -> Complex Double)
  -> TidalConstants -> Vector (Complex Double)
getConst field (TidalConstants {consts = c}) = V.fromList $ map field c

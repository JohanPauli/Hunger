{- |
  Newton-Raphson method for solving the power flow problem. Uses an
  approximated Jacobian.
-}
module Modelling.PowerFlow.Newton
(
-- * Solve a Power Flow Problem
  solvePF
) where



-- Electrical types, vectors, and matrices:
import Util.Types
import Util.Vector (Vector)
import qualified Util.Vector as V
import Util.Matrix (Matrix, (#>))
import qualified Util.Matrix as M

-- Non-linear root-finding:
import Util.Root



-- | Solve a power flow problem using the Newton-Raphson method.
solvePF :: Int -- ^ Number of PQ buses.
        -> Int -- ^ Number of PV buses.
        -> Vector CPower -- ^ Initial power values.
        -> Vector CVoltage -- ^ Initial voltage values.
        -> Matrix CAdmittance -- ^ Admittance matrix.
        -> (Int, Vector CVoltage)
solvePF nPQ nPV s0 v0 adm =
  (length result, (collect . last) result)
  where
    result = root QuasiNewton 1e-8 powMis v0'

    collect v = V.zipWith (:+) re im V.++ V.drop l v0
      where (re,im) = V.splitAt (div (V.length v) 2) v
    v0' = V.map realPart vPQPV V.++ V.map imagPart vPQPV
      where vPQPV = V.take l v0

    -- Power mismatch is the function for which the root is found.
    l = nPQ + nPV
    g = M.cmap realPart (M.subMatrix (0,0) (l,l) adm)
    b = M.cmap imagPart (M.subMatrix (0,0) (l,l) adm)
    gSL = M.cmap realPart (M.dropColumns l $ M.takeRows l adm)
    bSL = M.cmap imagPart (M.dropColumns l $ M.takeRows l adm)
    eSL = V.map realPart (V.drop l v0)
    fSL = V.map imagPart (V.drop l v0)
    gebfSL = gSL #> eSL - bSL #> fSL
    gfbeSL = gSL #> fSL + bSL #> eSL
    p = V.map realPart (V.take l s0)
    qv = V.map imagPart (V.take nPQ s0)
      V.++ V.map ((^(2::Int)) . magnitude) (V.slice nPQ nPV v0)
    powMis v = pMis V.++ qvMis
      where
        pMis = p - e * gebf - f * gfbe
        qvMis = qv - (qCalc V.++ vCalc)
        qCalc = V.take nPQ f * V.take nPQ gebf - V.take nPQ e * V.take nPQ gfbe
        vCalc = V.map (^(2::Int)) (V.slice nPQ nPV e)
          + V.map (^(2::Int)) (V.slice nPQ nPV f)
        gebf = g #> e - b #> f + gebfSL
        gfbe = g #> f + b #> e + gfbeSL
        (e,f) = V.splitAt (div (V.length v) 2) v

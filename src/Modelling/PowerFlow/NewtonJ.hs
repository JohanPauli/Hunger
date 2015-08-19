{- |
  Newton-Raphson methodd for solving the power flow problem. Includes an
  analytic Jacobian for the system.
-}
module Modelling.PowerFlow.NewtonJ
(
-- * Solve a Power Flow Problem
  solvePF
) where



-- Electrical types, vectors, and matrices:
import Util.Types
import Util.Vector (Vector)
import qualified Util.Vector as V
import Util.Matrix (Matrix, (#>), (!))
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
    result = rootJ Newton 1e-8 powMis pfJacobian v0'

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

    -- Cheating and simulating for loops with folds.
    gVec = M.flatten g
    bVec = M.flatten b
    pfJacobian v =
      V.foldl' addRow (M.konst (0::Double) (2*l,2*l)) (V.enumFromN 0 (2*l))
      where
        gebf = g #> e - b #> f + gebfSL
        gfbe = g #> f + b #> e + gfbeSL
        ge = M.takeDiag g * e
        gf = M.takeDiag g * f
        be = M.takeDiag b * e
        bf = M.takeDiag b * f
        dPde = M.reshape l (-gVec * es - bVec * fs)
        dPdf = M.reshape l (-gVec * fs + bVec * es)
        es = V.concatMap (V.replicate l) e
        fs = V.concatMap (V.replicate l) f
        (e,f) = V.splitAt (div (V.length v) 2) v
        addRow m i = V.foldl' (addEntry i) m (V.enumFromN 0 (2*l))
        addEntry i m j
          -- 1st quadrant.
          -- dPde, i==j.
          | i<l && i==j = ud (-gebf!i - ge!i - bf!i)
          -- dPde, i/=j.
          | i<l && j<l = ud (dPde!i!j)

          -- 2nd quadrant.
          -- dPdf, i==j.
          | i<l && (i+l)==j = ud (-gfbe!i - gf!i + be!i)
          -- dPdf, i/=j.
          | i<l = ud (dPdf!i!(j-l))

          -- 3rd quadrant.
          -- dQde, i==j.
          | i<(l+nPQ) && i==(j+l) = ud (gfbe!(i-l) - gf!(i-l) + be!(i-l))
          -- dQde, i/=j.
          | i<(l+nPQ) && j<l = ud (dPdf!(i-l)!j)
          -- dVde, i==j.
          | i==(j+l) = ud (-2*e!(i-l))
          -- dVde, i/=j.
          | j<l = ud 0

          -- 4th quadrant.
          -- dQdf, i==j.
          | i<(l+nPQ) && i==j = ud (-gebf!(i-l) + ge!(i-l) + bf!(i-l))
          -- dQdf, i/=j.
          | i<(l+nPQ) = ud (-dPde!(i-l)!(j-l))
          -- dVdf, i==j.
          | i==j = ud (-2*f!(i-l))
          -- dVdf, i/=j.
          | otherwise = ud 0
          where
            ud = flip (M.update (i,j)) m

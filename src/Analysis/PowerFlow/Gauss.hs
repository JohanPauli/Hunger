{- |
  The Gauss-Seidel power flow method. Not very functional.
-}
module Analysis.PowerFlow.Gauss
(
-- * Gauss-Seidel power flow
  solvePF
)
where



-- Vectors:
import Data.VecMat (Matrix, Vector, (!), (#>), (?), (#>), conj, dot)
import qualified Data.VecMat as V
-- Local:
import Data.Grid.Types



-- | Solve a power flow problem by the Gauss-Seidel method.
solvePF :: Int -- ^ Number of PQ buses.
        -> Int -- ^ Number of PV buses.
        -> (Vector CPower, Vector CVoltage) -- ^ Initial values.
        -> Matrix CAdmittance -- ^ Admittance matrix.
        -> (Vector CPower, Vector CVoltage, Int)
solvePF n m (s0,v0) adm =
  gaussStep (s0, v0, 1)
  where
    -- An iteration step of the Gauss-Seidel variety.
    gaussStep :: (Vector CPower, Vector CVoltage, Int)
              -> (Vector CPower, Vector CVoltage, Int)
    gaussStep (s,v,it)
      | mis < 1e-8 = (v * conj (adm #> v),v,it)
      | it >= 1000 = error $ V.dispcf 2 $ V.asRow $ s - v * conj (adm #> v)
      | otherwise = gaussStep (s',v',it+1)
      where
        (s',v') = (updatePV . updatePQ) (s, v)
        mises = V.init $ s - v * conj (adm #> v)
        realMis = V.norm_Inf $ V.map realPart mises
        imagMis = V.norm_Inf $ V.map imagPart mises
        mis = max realMis imagMis

    -- Update the PQ buses using Gauss-Seidel.
    updatePQ :: (Vector CPower, Vector CVoltage)
             -> (Vector CPower, Vector CVoltage)
    updatePQ (s,v) = (s, foldl stepV v [1..n])
      where
        stepV u i = V.updateV i newU u
          where
            newU = ui + (conjugate (si/ui) - dot admRow u) / admii
            ui = u!(i-1)
            si = s!(i-1)
            admRow = V.flatten $ adm?[i-1]
            admii = adm!(i-1)!(i-1)

    -- Update the PV buses using Gauss-Seidel.
    updatePV :: (Vector CPower, Vector CVoltage)
             -> (Vector CPower, Vector CVoltage)
    updatePV (s,v) = foldl stepQV (s,v) [n+1..n+m]
      where
        stepQV (q,u) i = (q',u')
          where
            --magn = V.map (\x -> magnitude x:+0)
            vmi = magnitude (v0!(i-1)):+0
            ui = u!(i-1)
            qi = q!(i-1)
            admRow = V.flatten $ adm?[i-1]
            admii = adm!(i-1)!(i-1)
            newS = realPart qi :+ imagPart (ui * conjugate (dot admRow u))
            q' = V.updateV i newS q
            newU = ui + (conjugate (newS/ui) -  dot admRow u) / admii
            newU' = vmi * newU / (magnitude newU:+0)
            u' = V.updateV i newU' u

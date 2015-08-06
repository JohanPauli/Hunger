{- |
  The Gauss-Seidel power flow method. Not very functional.
-}
module Modelling.PowerFlow.Gauss
(
-- * Gauss-Seidel power flow
  solvePF
)
where



-- Foldables:
import qualified Data.Foldable as F

-- Electrical types, vectors, and matrices:
import Util.Types
import Util.Vector (Vector, dot)
import Util.Matrix (Matrix, (!), (?), (#>), conj)
import qualified Util.Vector as V
import qualified Util.Matrix as M



-- | Solve a power flow problem by the Gauss-Seidel method.
solvePF :: Int -- ^ Number of PQ buses.
        -> Int -- ^ Number of PV buses.
        -> Vector CPower -- ^ Initial power values.
        -> Vector CVoltage -- ^ Initial voltage values.
        -> Matrix CAdmittance -- ^ Admittance matrix.
        -> (Int, Vector CVoltage)
solvePF n m s0 v0 adm =
  let (_,vRes,itRes) = gaussStep (s0, v0, 1) in (itRes,vRes)
  where
    -- An iteration step of the Gauss-Seidel variety.
    gaussStep :: (Vector CPower, Vector CVoltage, Int)
              -> (Vector CPower, Vector CVoltage, Int)
    gaussStep (s,v,it)
      | mis < 1e-8 = (v * conj (adm #> v),v,it)
      | it >= 1000 = error $ M.dispcf 2 $ M.asRow $ s - v * conj (adm #> v)
      | otherwise = gaussStep (s',v',it+1)
      where
        -- Updated values for s and v.
        (s',v') = (updatePV . updatePQ) (s, v)
        -- The convergence criterion parameter, 'power mismatch.'
        mis = M.norm_Inf $ V.init $ s - v * conj (adm #> v)

    -- Update the PQ buses using Gauss-Seidel.
    updatePQ :: (Vector CPower, Vector CVoltage)
             -> (Vector CPower, Vector CVoltage)
    updatePQ (s,v) = (s, foldl stepV v [0..n-1])
      where
        stepV u i = V.update i newU u
          where
            newU = ui + (conjugate (si/ui) - dot admRow u) / admii
            ui = u!i
            si = s!i
            admRow = M.flatten $ adm?[i]
            admii = adm!i!i

    -- Update the PV buses using Gauss-Seidel.
    updatePV :: (Vector CPower, Vector CVoltage)
             -> (Vector CPower, Vector CVoltage)
    updatePV (s,v) = F.foldl' stepQV (s,v) [n..n+m-1]
      where
        stepQV (q,u) i = (q',u')
          where
            --magn = V.map (\x -> magnitude x:+0)
            vmi = magnitude (v0!i):+0
            ui = u!i
            qi = q!i
            admRow = M.flatten $ adm?[i]
            admii = adm!i!i
            newS = realPart qi :+ imagPart (ui * conjugate (dot admRow u))
            q' = V.update i newS q
            newU = ui + (conjugate (newS/ui) -  dot admRow u) / admii
            newU' = vmi * newU / (magnitude newU:+0)
            u' = V.update i newU' u

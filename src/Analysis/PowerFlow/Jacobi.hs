{- |
  The Jacobi method for solving power flow.
-}
module Analysis.PowerFlow.Jacobi
(
-- * Jacobi power flow solver.
  solvePF
)
where



-- Prelude hiding:
import Prelude hiding ((++))

-- Vectors:
import Data.VecMat (Matrix, Vector, (!), (#>), conj)
import qualified Data.VecMat as V

-- Local:
import Data.Grid.Types



-- | Solve a power flow problem using the Jacobi method.
solvePF :: Int -- ^ Number of PQ buses.
        -> Int -- ^ Number of PV buses.
        -> (Vector CPower, Vector CVoltage) -- ^ Initial values.
        -> Matrix CAdmittance -- ^ Admittance matrix.
        -> (Vector CPower, Vector CVoltage, Int)
solvePF n m (s0,v0) adm =
  if V.norm_Inf (s0 - v0 * conj (adm #> v0)) < 1e-3
  then (s0, v0, 1)
  else jacobiStep (s0, v0, 1)
  where
    -- A step in the jacobi iteration algorithm.
    jacobiStep :: (Vector CPower, Vector CVoltage, Int)
               -> (Vector CPower, Vector CVoltage, Int)
    jacobiStep (s,v,it)
      | mis < 1e-5 = (v * conj (adm #> v),v,it)
      | it > 1000 = error $ V.dispcf 2 $ V.asRow $ s - v * conj (adm #> v)
      | otherwise = jacobiStep (s',v',it+1)
      where
        (s',v') = (updatePQPV . updatePV) (s,v)
        mises = V.init $ s - v * conj (adm #> v)
        realMis = V.norm_Inf $ V.map realPart mises
        imagMis = V.norm_Inf $ V.map imagPart mises
        mis = max realMis imagMis

    -- Elementwise reciprocal to the diagonal of the admittance matrix.
    d = V.cmap (1/) $ V.takeDiag adm

    -- A new voltage estimation uses an estimated power and an older estimated
    -- voltage.
    updatePQPV :: (Vector CPower, Vector CVoltage)
               -> (Vector CPower, Vector CVoltage)
    updatePQPV (s,v) =
      (s, V.updateV (n+m+1) (v0!(n+m)) (v + d * (conj (s/v) - adm #> v)))

    -- A new reactive power estimation uses PV bus data.
    updatePV :: (Vector CPower, Vector CVoltage)
             -> (Vector CPower, Vector CVoltage)
    updatePV (s,v) = (updatedQ,v)
      where
        qPV = V.map imagPart $ v * conj (adm #> v)
        addQ q i = V.updateV i
          (realPart (q!(i-1)) :+ qPV!(i-1)) q
        updatedQ = Prelude.foldl addQ s [n+1..n+m]

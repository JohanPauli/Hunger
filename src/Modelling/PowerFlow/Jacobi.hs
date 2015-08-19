{- |
  The Jacobi method for solving power flow.
-}
module Modelling.PowerFlow.Jacobi
(
-- * Jacobi power flow solver.
  solvePF
) where



-- Electrical types and vectors/matrices:
import Util.Types
import Util.Vector (Vector)
import qualified Util.Vector as V
import Util.Matrix (Matrix, (!), (#>), conj)
import qualified Util.Matrix as M



-- | Solve a power flow problem using the Jacobi method.
solvePF :: Int -- ^ Number of PQ buses.
        -> Int -- ^ Number of PV buses.
        -> Vector CPower -- ^ Initial power values.
        -> Vector CVoltage -- ^ Initial voltage values.
        -> Matrix CAdmittance -- ^ Admittance matrix.
        -> (Int, Vector CVoltage)
solvePF n m s0 v0 adm =
  let (_,sRes,itRes) = jacobiStep (s0, v0, 1) in (itRes,sRes)
  where
    -- A step in the jacobi iteration algorithm.
    jacobiStep :: (Vector CPower, Vector CVoltage, Int)
               -> (Vector CPower, Vector CVoltage, Int)
    jacobiStep (s,v,it)
      | mis < 1e-7 = (v * conj (adm #> v),v,it)
      | it > 5000 = error $ M.dispcf 2 $ M.asRow $ s - v * conj (adm #> v)
      | otherwise = jacobiStep (s',v',it+1)
      where
        -- Updated values for s and v.
        (s',v') = (updatePVV . updatePQPV . updatePVQ) (s,v)
        -- The convergence criterion parameter, 'power mismatch.'
        mis = M.norm_Inf $ V.init $ s - v * conj (adm #> v)

    -- Elementwise reciprocal to the diagonal of the admittance matrix.
    d = V.map (1/) $ M.takeDiag adm

    -- A new voltage estimation uses an estimated power and an older estimated
    -- voltage.
    updatePQPV :: (Vector CPower, Vector CVoltage)
               -> (Vector CPower, Vector CVoltage)
    updatePQPV (s,v) =
      (s, V.update (n+m) (v0!(n+m-1)) (v + d * (conj (s/v) - adm #> v)))

    -- A new reactive power estimation uses PV bus data.
    updatePVQ :: (Vector CPower, Vector CVoltage)
             -> (Vector CPower, Vector CVoltage)
    updatePVQ (s,v) = (updatedQ,v)
      where
        qPV = V.map imagPart $ v * conj (adm #> v)
        addQ q i = V.update i (realPart (q!i) :+ qPV!i) q
        updatedQ = V.foldl' addQ s (V.enumFromN n m)

    updatePVV :: (Vector CPower, Vector CVoltage)
             -> (Vector CPower, Vector CVoltage)
    updatePVV (s,v) = (s,updatedV)
      where
        vm = V.map (\x -> magnitude x:+0) v0
        addV u i = V.update i ((vm!i) * (u!i)/(magnitude (u!i):+0)) u
        updatedV = V.foldl' addV v (V.enumFromN n m)

{- |
  Iterative methods for finding roots of nonlinear systems.

  Interface similar to hmatrix-gsl.
-}
module Util.Root
(
-- * Utilities
  within

-- * Root finidng not supplying Jacobian.
, Method (..)
, root

-- * Root finding supplying Jacobian.
, MethodJ (..)
, rootJ
) where



-- Vectors and matrices:
import Util.Vector (Vector, Storable)
import qualified Util.Vector as V
import Util.Matrix (Matrix, Field)
import qualified Util.Matrix as M

-- Differentiation (gradient and jacobian):
import Util.Diff




-- Utilities.
-- | Tolerance checker.
within :: (Storable a, Num a, Ord a) => a -> [Vector a] -> [Vector a]
within eps (x:y:rest)
  | V.sum (V.map abs $ V.zipWith (-) x y) <= eps = [y]
  | otherwise = x:within eps (y:rest)
within _ _ = error "Using 'within' on non-infinite list"



-- Methods without Jacobian.
-- | Solution methods for nonlinear roots without provided Jacobian.
data Method =
    QuasiNewton -- ^ The standard Newton-Raphson with naively approx. Jacobian.

-- | Find a root of a vector function based on an initial guess.
root :: (Fractional a, Ord a, Field a)
     => Method -- ^ `Method` used for finding the root.
     -> a -- ^ Tolerance for the root (small number).
     -> (Vector a -> Vector a) -- ^ Vector function to find roots for
     -> Vector a -- ^ Initial guess.
     -> [Vector a ] -- ^ Result.
root QuasiNewton eps f x = Prelude.take 10 $
  within eps (iterate stepNewton x)
  where
    stepNewton x' = getRes x' $ M.linearSolve (jacobian f x') (minusF x')
    getRes x' Nothing = x'
    getRes x' (Just res) = V.zipWith (+) x' (M.flatten res)
    minusF x' = M.fromColumns [V.map negate $ f x']



-- | Find a root of a vector function based on its Jacobian
-- and an initial guess.
data MethodJ = Newton -- ^ The Newton-Raphson method given a Jacobian.


-- | Find a root of a vector function based on an initial guess and the
-- function's Jacobian.
rootJ :: (Fractional a, Ord a, Field a)
     => MethodJ -- ^ `Method` used for finding the root.
     -> a -- ^ Tolerance for the root (small number).
     -> (Vector a -> Vector a) -- ^ Vector function to find roots for.
     -> (Vector a -> Matrix a) -- Jacobian of vector function.
     -> Vector a -- ^ Initial guess.
     -> [Vector a] -- ^ Result.
rootJ Newton eps f j x = within eps (iterate stepNewton x)
  where
    stepNewton x' = getRes x' $ M.linearSolve (j x') (minusF x')
    getRes x' Nothing = x'
    getRes x' (Just res) = V.zipWith (+) x' (M.flatten res)
    minusF x' = M.fromColumns [V.map negate $ f x']

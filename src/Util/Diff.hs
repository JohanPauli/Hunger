{- |
  Differentiation helpers for Vectors.
-}
module Util.Diff
(
-- * Gradients
  grad

-- * Jacobians
, jacobian
) where



-- Vectors and matrices:
import Util.Vector (Vector, Storable, (!))
import qualified Util.Vector as V
import Util.Matrix (Matrix, reshape)



-- | Linear approximation of the gradient.
grad :: (Storable a, Fractional a, Eq a)
     => (Vector a -> a)
     -> Vector a
     -> Vector a
grad f x = V.unfoldr unfoldGrad m
  where
    -- Make a vector of function changes with each corresponding x entry
    -- slightly changed (linear approximation to the gradient).
    unfoldGrad 0 = Nothing
    unfoldGrad n = Just ((f' n - fx) / eps,n-1)

    -- Get the function value with the n'th entry of x slightly changed.
    f' n = f (V.update (m-n) ((x!(m-n))+eps) x)

    -- A precomputed non-modified result and length.
    fx = f x
    m = V.length x
    eps = 1e-8

-- | Linear approximation to the jacobian.
--
-- This uses `grad` on each row of the function.
jacobian :: (Storable a, Fractional a, Eq a)
         => (Vector a -> Vector a)
         -> Vector a
         -> Matrix a
jacobian f x =
  reshape n $ V.concatMap mkVec (V.enumFromN 0 n)
  where
    -- Each row is a gradient.
    mkVec i = grad (\x' -> f x'!i) x
    n = V.length (f x)

{- |
  Vector and matrix compatibility layer. Exports "Data.Vector.Storable"
  for vectors and related operations and "Numeric.LinearAlgebra.HMatrix"
  for linear algebra operations and matrices.
-}
module Data.VecMat
(
-- * Update functions
  updateV
, updateM
, dot

-- * Imports
, module M
, module V
)
where



-- Vectors:
import Data.Vector.Storable as V hiding
  (minIndex, maxIndex, find, accum, toList, (!))
import Data.Vector.Storable.Mutable (write)

-- Matrices and linear algebra operations:
import Numeric.LinearAlgebra.HMatrix as M hiding (dot)
import Numeric.LinearAlgebra.Devel (runSTMatrix, writeMatrix, thawMatrix)



-- | Update a storable vector at (1-indexed) index.
updateV :: (Storable a) => Int -> a -> Vector a -> Vector a
updateV i x = V.modify (\mv -> write mv (i-1) x)

-- | Update a matrix efficiently.
updateM :: (Storable a) => (Int,Int) -> a -> Matrix a -> Matrix a
updateM (i,j) x m = runSTMatrix $ do
  m' <- thawMatrix m
  writeMatrix m' (i-1) (j-1) x
  return m'

-- | Temporary substitute for the HMatrix dot product, as it seems to be
-- broken in hmatrix-0.16.1.5.
dot :: (Storable a, Num a) => Vector a -> Vector a -> a
dot u v = V.sum $ V.zipWith (*) u v

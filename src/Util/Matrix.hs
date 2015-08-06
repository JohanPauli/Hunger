{- |
  Matrix compatibility layer. Really the matrices from HMatrix.

  The matrices are proper matrices in real or complex numbers.

  This also exports a wealth of linear algebra operations.
-}
module Util.Matrix
(
-- * Update functions
  update

-- Imports
, module M
)
where



-- Storableness:
import Data.Vector.Storable (Storable)

-- Matrices and linear algebra operations:
import Numeric.LinearAlgebra.HMatrix as M hiding (dot)
import Numeric.LinearAlgebra.Devel (runSTMatrix, writeMatrix, thawMatrix)



-- | Update a (0-indexed) matrix efficiently.
update :: (Storable a) => (Int,Int) -> a -> Matrix a -> Matrix a
update (i,j) x m = runSTMatrix $ do
  m' <- thawMatrix m
  writeMatrix m' i j x
  return m'

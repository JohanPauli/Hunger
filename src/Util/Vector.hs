{- |
  Vector compatibility layer. Really storable vectors from
  "Data.Vector.Storable".

  The vectors here are proper, numeric vectors, either in real or complex
  numbers.
-}
module Util.Vector
(
-- * Update Functions
  update
, dot

-- Imports
, module V
)
where



-- Vectors:
import Data.Vector.Storable as V hiding
  (minIndex, maxIndex, find, accum, toList)
import Data.Vector.Storable.Mutable (write)



-- | Update a storable vector at (0-indexed) index.
update :: (Storable a) => Int -> a -> Vector a -> Vector a
update i x = V.modify (\mv -> write mv i x)

-- | Temporary substitute for the HMatrix dot product, as it seems to be
-- broken in hmatrix-0.16.1.5.
dot :: (Storable a, Num a) => Vector a -> Vector a -> a
dot u v = V.sum $ V.zipWith (*) u v

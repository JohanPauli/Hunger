module Util.Sparse.Matrix
(
-- * The sparse matrix datatype
  Matrix (..)

-- * Binary operators
-- ** Matrix-scalar operations
, (!+), (!-), (!*), (!/)
, (+!), (-!), (*!), (/!)
-- ** Matrix-vector operations
, (!*.) --, (.*!)
-- ** Matrix-matrix operations
, (!+!), (!-!), (!*!)

-- * Accessors
-- ** Length
, rows, cols
-- ** Indexing
, (#), col, diagonalBlocks
-- ** Extracting submatrices or vectors (slicing)
, diagonal, lowerTriangle, upperTriangle

-- * Construction
-- ** Initialisation
, empty, zero, setSize
-- ** Concatenation
, (!++!)
, addRow

-- * Modifications
-- ** Insertion
, insNum, insNums, insertAllWith

-- * Elementwise operations
, mapRows

-- * List conversions
, toList, fromList
) where



-- List processing:
import Data.List as L

-- | Applicative, for neatness:
import Control.Applicative ((<$>))

-- This is a monoid:
import Data.Monoid

-- Local:
import Util.Sparse.Vector (Vector (..),(!))
import qualified Util.Sparse.Vector as V



-- | A sparse matrix is just a spare vector of sparse vectors with
-- additional information about dimensions.
data Matrix a =
  Matrix
  { dims :: (Int,Int)
  , mat :: Vector (Vector a) }

-- | `fmap` maps a function over all the non-zero entries of the matrix.
--
-- This doesn't really make sense though (even if it is what is done in
-- sparse-lin-alg).
instance Functor Matrix where
  fmap f (Matrix d m) = Matrix d (fmap (fmap f) m)

-- | `mappend` is vertical concatenation (adding rows to the matrix).
instance Monoid (Matrix a) where
  mempty = empty
  mappend = (!++!)

-- | `show` gives the dimensions and prints the constituent vectors.
instance (Show a, Num a, Eq a) => Show (Matrix a) where
  show m = show (dims m) ++ ":\n" ++ doShow (mat m)
    where
      doShow m' = unlines (V.toListWithDefault emptyRow (fmap show m'))
      emptyRow = show (snd $ dims m) ++ ": ["
        ++ L.intercalate "|" (replicate (snd $ dims m) ".") ++ "] (non-ext)"



-- Binary operators
-- Matrix-scalar operations.
infixl 7 !*,*!,!/,/!
infixl 6 !+,+!,!-,-!
-- | Elementwise addition (matrix on the left).
(!+) :: (Num a, Eq a) => Matrix a -> a -> Matrix a
(!+) m x = insertAllWith (+x) m

-- | Elementwise subtraction of scalar from matrix.
(!-) :: (Num a, Eq a) => Matrix a -> a -> Matrix a
(!-) m x = insertAllWith (subtract x) m

-- | Elementwise multiplication (matrix on the left).
(!*) :: (Num a) => Matrix a -> a -> Matrix a
(!*) m x = fmap (* x) m

-- | Elementwise division of matrix by scalar.
(!/) :: (Fractional a) => Matrix a -> a -> Matrix a
(!/) m x = fmap (/ x) m

-- | Elementwise addition (matrix on the right).
(+!) :: (Num a, Eq a) => a -> Matrix a -> Matrix a
(+!) x m = m !+ x

-- | Elementwise subtraction of matrix from scalar.
(-!) :: (Num a, Eq a) => a -> Matrix a -> Matrix a
(-!) x m = m !- x

-- | Elementwise multiplication (matrix on the right).
(*!) :: (Num a) => a -> Matrix a -> Matrix a
(*!) x m = m !* x

-- | Elementwise division of scalar by matrix.
(/!) :: (Fractional a) => a -> Matrix a -> Matrix a
(/!) x m = m !/ x



-- Matrix-vector operations.
-- | Multiply matrix by vector (matrix product).
--
-- The vector is implicitly a column vector.
infixl 7 !*.
(!*.) :: (Num a) => Matrix a -> Vector a -> Vector a
(!*.) m v = mapFoldRows (f v) m
  where
    mapFoldRows :: (Vector a -> a) -> Matrix a -> Vector a
    mapFoldRows f' (Matrix _ m') = fmap f' m'
    f :: (Num a) => Vector a -> Vector a -> a
    f v' w' = V.sum (v' V..*. w')



-- Matrix-matrix operations.
infixl 7 !*!
infixl 6 !+!,!-!
-- | Elementwise addition of matrices.
(!+!) :: (Num a) => Matrix a -> Matrix a -> Matrix a
(!+!) = unionWith (+)

-- | Elementwise subtraction of matrices.
(!-!) :: (Num a) => Matrix a -> Matrix a -> Matrix a
(!-!) m n = unionWith (+) m (fmap negate n)

-- | Elementwise multiplication of matrices.
(!*!) :: (Num a) => Matrix a -> Matrix a -> Matrix a
(!*!) = intersectionWith (*)


-- Accessors
-- Length
rows :: Matrix a -> Int
rows = fst . dims

cols :: Matrix a -> Int
cols = snd . dims



-- Indexing
-- | Index the matrix.
(#) :: (Num a) => Matrix a -> (Int, Int) -> a
(#) (Matrix (x,y) m) (i,j)
  | (x,y) < (i,j) = error "Matrix index out of bounds!"
  | otherwise = V.indexWithDefault m (V.zeroV y) i ! j

-- | Get a column out of the matrix.
col :: Int -> Matrix a -> Vector a
col i (Matrix (_,y) m) = V.indexWithDefault m (V.zeroV y) i

-- | Get the matrix left/above and the matrix right/below.
diagonalBlocks :: (Int,Int) -> Matrix a -> (Matrix a, Matrix a)
diagonalBlocks (xNew,yNew) (Matrix (x,y) m) =
  ( Matrix (xNew-1,yNew-1) b1
  , Matrix (x,y) b4)
  where
    (h1,h2) = V.splitAtExcl xNew m
    (b1, _) = V.splitAtExcl yNew h1
    ( _,b4) = V.splitAtExcl yNew h2



-- Slicing
-- | Extracts the diagonal elements of a matrix.
diagonal :: (Num a, Eq a) => Matrix a -> Vector a
diagonal m =
  let newDim = uncurry min $ dims m in collect newDim (V.zeroV newDim)
  where
    --collect :: Int -> Vector a -> Vector a
    collect n' v'
      | n' >= 1 = collect (n'-1) (V.insNum n' (m#(n',n')) v')
      | otherwise = v'

-- | Decomposes the matrix into a strict lower triangular matrix and the rest.
lowerTriangle :: (Eq a) => Matrix a -> (Matrix a, Matrix a)
lowerTriangle (Matrix (x,y) m)
  | x /= y = error "Trying to decompose non-square matrix!"
  | otherwise = (Matrix (x,y) lowar, Matrix (x,y) uppar)
  where
    uppar = fmap (V.setSize y) (V.setSize x ur)
    lowar = fmap (V.setSize y) (V.setSize x lr)
    (lr,ur) = V.unzip (f m)
    f = V.mapWithKey (\i v -> V.splitAt (i-1) v)


-- | Decomposes the matrix into a strict upper triangular matrix and the rest.
upperTriangle :: (Eq a) => Matrix a -> (Matrix a, Matrix a)
upperTriangle (Matrix (x,y) m)
  | x /= y = error "Trying to decompose non-square matrix!"
  | otherwise = (Matrix (x,y) lower, Matrix (x,y) upper)
  where
    upper = fmap (V.setSize y) (V.setSize x ur)
    lower = fmap (V.setSize y) (V.setSize x lr)
    (lr,ur) = V.unzip (f m)
    f = V.mapWithKey V.splitAt



-- Construction
-- | Construct an empty matrix.
empty :: Matrix a
empty = Matrix (0,0) V.empty

-- | Construct a matrix 'filled' with zeros.
zero :: (Int,Int) -> Matrix a
zero (x,y) = Matrix (x,y) (V.zeroV y)

-- | Vertically concatenate two matrices.
(!++!) :: Matrix a -> Matrix a -> Matrix a
(!++!) (Matrix (x1,y1) m1) (Matrix (x2,y2) m2)
  | y1 /= y2 = error "Concatenating incompatible matrices!"
  | otherwise = Matrix (x1+x2,y1) (mappend m1 m2)

-- | Add a row to the Matrix.
addRow :: Vector a -> Matrix a -> Matrix a
addRow v (Matrix (x,y) m)
  | y == V.dim v = Matrix (x+1,y) (m V..++. V.singleton v)
  | otherwise = error "Adding row of wrong length to matrix."



-- Insertions
-- | Set the size of the matrix, removing items with higher indices.
setSize :: (Int,Int) -> Matrix a -> Matrix a
setSize (x,y) (Matrix _ m) =
  Matrix (x,y) (V.setSize y <$> V.setSize x m)

-- | Insert a single numeric item into a Matrix. Expands when out of bounds.
insNum :: (Num a, Eq a) => (Int,Int) -> a -> Matrix a -> Matrix a
insNum (i,j) val (Matrix (x,y) m) =
  Matrix (max i x, max y j) newVecVec
  where
    newVecVec = V.insMonoid i newVec m
    newVec = V.insNum j val (V.indexWithDefault m (V.zeroV y) i)

-- | Insert multiple numbers into a matrix.
insNums :: (Num a, Eq a) => [((Int,Int),a)] -> Matrix a -> Matrix a
insNums xs m = foldl (flip (uncurry insNum)) m xs

-- | Applies an unary function (i.e. a partially applied function) to
-- every index. Zeros are omitted.
insertAllWith :: (Eq a, Num a) => (a -> a) -> Matrix a -> Matrix a
insertAllWith f (Matrix d m) = Matrix d (V.insertAllWithDefault fRow V.empty m)
  where fRow = V.insertAllWithDefault f 0



-- Combinations
-- | Per-element union with a function on intersection.
unionWith :: (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
unionWith f (Matrix d m) (Matrix h n)
  | d /= h = error "Uniting matrices of different sizes!"
  | otherwise = Matrix d (V.unionWith (V.unionWith f) m n)

-- | Per-element intersection with a function.
intersectionWith :: (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
intersectionWith f (Matrix d m) (Matrix h n)
  | d /= h = error "Intersecting matrices of different sizes!"
  | otherwise = Matrix d (V.intersectionWith (V.intersectionWith f) m n)





-- Elementwise operations.
-- Map a vector function over each non-zero row of of the matrix.
mapRows :: (Vector a -> Vector b) -> Matrix a -> Matrix b
mapRows f (Matrix d m) = Matrix d (fmap f m)






-- List conversions
-- | Converts plain list-matrix to sparse matrix, throwing out all zeroes
fromList :: (Num a, Eq a) => [[a]] -> Matrix a
fromList [] = empty
fromList m@(r:_) = Matrix (length m, length r) $ V.fromAssocList
    [ (i,row) | (i,row) <- zipWith pair [1..] m, not (V.isEmpty row) ]
    where pair i r' = (i, V.fromList r')

-- | Make a list of lists from a matrix, filling in zeros.
toList :: (Num a) => Matrix a -> [[a]]
toList m = [ [ m # (i,j) | j <- [1 .. rows  m] ]
                         | i <- [1 .. cols m] ]

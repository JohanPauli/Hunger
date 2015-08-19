{- |
  An implementation of sparse vectors as IntMap's.

  Very like the sparse-lin-alg package vectors, except more like the vector
  package vectors in usage.
-}

module Util.Sparse.Vector
(
-- * The sparse vector data type
  Vector (..)

-- * Binary operators
-- ** Vector-scalar operators
, (.+), (.-), (.*), (./)
, (+.), (-.), (*.), (/.)
-- ** Vector-vector operators
, (.+.), (.-.), (.*.), (./.), (.**.)

-- * Accessors
-- ** Length
, length, isEmpty
-- ** Indexing
, (!), head, last, indexWithDefault
-- ** Extracting subvectors (slicing)
-- , slice, init, tail, take, drop
, splitAt, splitAtExcl

-- * Construction
-- ** Initialisation
, empty, zeroV, singleton, replicate --, generate
-- ** Concatenation
, (.++.)
-- ** Unfolding
-- , unfoldr, unfoldrN, constructN, constructrN

-- ** Insertion
, setSize, insZero, insNum, insMonoid, insertWithN, insertAllWithDefault

-- * Combinations
-- ** Union
, unionWith
-- ** Intersection
, intersectionWith

-- * Elementwise operations.
, mapWithKey

-- * Folding
, foldr, foldWithKey, unzip, sum, dotP

-- * List conversions
, fromList, fromAssocList, toList, toListWithDefault
) where


-- Prelude hiding and things:
import qualified Prelude as P
import Prelude hiding
  (length,replicate,head,last,tail,splitAt,drop,take,init,foldr,sum,unzip)

-- List processing:
import qualified Data.List as L

-- IntMaps:
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

-- Monoids and folds:
import Data.Monoid
import Data.Foldable (Foldable (..))



-- The vector data type.
-- | Vectors are implemented as `IntMap`s with additional storage of size.
data Vector a =
  Vector
  { dim :: Int -- ^ The dimension of the vector.
  , vec :: IntMap a -- ^ `IntMap` storage for the vector.
  } deriving (Eq)


-- | `fmap` applies f to each non-zero value.
instance Functor Vector where
  fmap f (Vector n v) = Vector n (fmap f v)

-- | `mappend` appends a vector to the end of the other.
instance Monoid (Vector a) where
  mempty = empty
  mappend = (.++.)

instance Foldable Vector where
  foldr = foldrV

instance (Num a) => Num (Vector a) where
  (+) = (.+.)
  (-) = (.-.)
  (*) = (.*.)
  abs = fmap abs
  signum = fmap signum
  fromInteger = singleton . fromInteger

instance (Show a,Num a, Eq a) => Show (Vector a) where
  show v
    | count v == 0 = showSparseList (toList v) ++ " (empty)"
    | otherwise = showSparseList $ toList v
    where
      showSparseList :: (Show a, Eq a, Num a) => [a] -> String
      showSparseList l = show (L.length l) L.++ ": [" L.++
        L.intercalate "|" (L.map showNonZero l) L.++ "]"
      showNonZero :: (Show a, Eq a, Num a) => a -> String
      showNonZero x = if x==0 then "." else show x

-- Vector-scalar operations.
infixl 7 .*, *., ./, /.
infixl 6 .+, +., .-, -.
-- | Elementwise addition of scalar and vector (vector on the left).
(.+) :: (Num a, Eq a) => Vector a -> a -> Vector a
(.+) v x = insertWithN (dim v) (+) x v

-- | Elementwise subtraction of scalar from vector.
(.-) :: (Num a, Eq a) => Vector a -> a -> Vector a
(.-) v x = insertWithN (dim v) (+) (negate x) v

-- | Elementwise multiplication of scalar and vector (vector on the left).
(.*) :: (Num a) => Vector a -> a -> Vector a
(.*) (Vector n v) x = Vector n (M.map (*x) v)

-- | Elementwise division of vector by scalar.
(./) :: (Fractional a, Eq a) => Vector a -> a -> Vector a
(./) (Vector n v) x
  | x == 0 = error "Division by 0!"
  | otherwise = Vector n (M.map (/x) v)

-- | Elementwise addition of scalar and vector (vector on the right).
(+.) :: (Num a, Eq a) => a -> Vector a -> Vector a
(+.) x v = v .+ x

-- | Elementwise subtraction of vector from scalar.
(-.) :: (Num a, Eq a) => a -> Vector a -> Vector a
(-.) x v = insertWithN (dim v) (+) x (fmap negate v)

-- | Elementwise multiplication of vector with scalar (vector on the right).
(*.) :: (Num a) => a -> Vector a -> Vector a
(*.) x v = v .* x

-- | Elementwise division of scalar by vector.
(/.) :: (Fractional a, Eq a) => a -> Vector a -> Vector a
(/.) x v = fmap (1/) (v ./ x)



-- Vector-Vector operations.
infixl 6 .+.,.-.
infixl 7 .*.,./.
-- | Elementwise addition.
(.+.) :: (Num a) => Vector a -> Vector a -> Vector a
(.+.) = unionWith (+)

-- | Elementwise subtraction.
(.-.) :: (Num a) => Vector a -> Vector a -> Vector a
(.-.) v w = unionWith (+) v (fmap negate w)

-- | Elementwise multiplication.
(.*.) :: (Num a) => Vector a -> Vector a -> Vector a
(.*.) = intersectionWith (*)

-- | Elementwise division.
(./.) :: (Fractional a) => Vector a -> Vector a -> Vector a
(./.) = intersectionWith (/)

-- | The dot product.
infixl 7 .**.
(.**.) :: (Num a) => Vector a -> Vector a -> a
(.**.) = dotP


-- Accessors
-- Length
-- | The length of the vector (including zeros).
length :: Vector a -> Int
length (Vector n _) = n

-- | Whether the length of the vector is 0.
isEmpty :: Vector a -> Bool
isEmpty (Vector 0 _) = True
isEmpty (Vector _ _) = False

-- | The number of non-zero elements in the vector.
count :: Vector a -> Int
count (Vector _ v) = M.size v



-- Indexing
-- | Indexing operator, defaults to 0.
(!) :: (Num a) => Vector a -> Int -> a
(!) (Vector n v) i
  | i > n = error "Index too large!"
  | i < 1 = error "Index too small!"
  | otherwise = M.findWithDefault 0 i v

-- | Extracts first non-zero element, defaults to 0.
head :: Vector a -> (Int, a)
head (Vector _ v) = M.findMin v

-- | Extracts the last non-zero elements, defaults to 0.
last :: Vector a -> (Int, a)
last (Vector _ v) = M.findMax v

indexWithDefault :: Vector a -> a -> Int -> a
indexWithDefault (Vector _ v) def i = M.findWithDefault def i v


-- Extracting subvectors (slicing)
-- | Take a number of elements from the first vector, and return the tuple
-- of that and the remaining elements as vectors.
--slice :: Int -> Vector a -> (Vector a, Vector a)
--slice = undefined

-- | Yield all but the last element of the vector.
--init :: Vector a -> Vector a
--init = undefined

-- | Yield all but the first element of the vector.
--tail :: Vector a -> Vector a
--tail = undefined

-- | Yield the first `n` elements.
--take :: Int -> Vector a -> Vector a
--take = undefined

-- | Yield all but the first `n` elements.
--drop :: Int -> Vector a -> Vector a
--drop = undefined

-- | Split the vector at a given index (keeping indices).
splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt idx (Vector n v) =
  let (first,atIdx,second) = M.splitLookup idx v
  in f first atIdx second
  where
    f first' Nothing second' = (Vector idx first', Vector (n-idx) second')
    f first' (Just atIdx') second' =
      ( Vector idx (M.insert idx atIdx' first')
      , Vector (n-idx) second' )

-- | Split the vector at a given index, exluding the element there.
splitAtExcl :: Int -> Vector a -> (Vector a, Vector a)
splitAtExcl idx (Vector n v) =
  ( Vector (idx-1) first
  , Vector (n-idx) second )
  where
    (first,second) = M.split idx v




-- Construction
-- Initialisation
-- | Construct an empty vector.
empty :: Vector a
empty = Vector 0 M.empty

-- | Construct a zero vector of length @n@.
zeroV :: Int -> Vector a
zeroV n = Vector n M.empty

-- | Construct a vector with a single element
singleton :: a -> Vector a
singleton x = Vector 1 (M.singleton 1 x)

-- | Construct a vector with the same element at every key up to a number.
replicate :: Int -- ^ Number of replications.
          -> a -- ^ Element to replicate.
          -> Vector a
replicate n x = Vector n (M.fromList [(i,x) | i <- [1..n]])



-- Concatenation
(.++.) :: Vector a -> Vector a -> Vector a
(.++.) (Vector n v) (Vector m w) =
  Vector (n+m) (insAll v (M.minViewWithKey w))
  where
    insAll :: IntMap a -> Maybe ((Int,a),IntMap a) -> IntMap a
    insAll v' Nothing = v'
    insAll v' (Just ((i,x),w')) =
      insAll (M.insert i x v') (M.minViewWithKey w')


-- Insertions
-- | Change the vector's size, removing items with higher indices.
setSize :: Int -> Vector a -> Vector a
setSize i v
  | i >= dim v = v {dim = i}
  | otherwise = fst $ splitAt i v

-- | Insertion giving the zero element.
insZero :: (Eq a) => Int -> a -> a -> Vector a -> Vector a
insZero i x z (Vector n v)
  | x == z = Vector n v
  | otherwise = Vector (max i n) (M.insert i x v)


-- | Insert a single value into a vector. May modify the vector's size.
insNum :: (Num a, Eq a) => Int -> a -> Vector a -> Vector a
insNum i x (Vector n v)
  | x == 0 = Vector n v
  | otherwise = Vector (max i n) (M.insert i x v)

-- | Insertion into a vector of a monoid (as opposed to, say, numbers).
--
-- This is for vectors of vectors and things like that.
insMonoid :: (Monoid a, Eq a) => Int -> a -> Vector a -> Vector a
insMonoid i x (Vector n v)
  | x == mempty = Vector n v
  | otherwise = Vector (max i n) (M.insert i x v)


-- | Insert into a vector with a function on, for the `n` first elements.
--
-- Mainly for use with `.+`, `+.`, `.-`, and `-.`.
insertWithN :: (Eq a, Num a)
            => Int
            -> (a -> a -> a)
            -> a
            -> Vector a
            -> Vector a
insertWithN n f x (Vector l v) =
  Vector (max l n) (M.filter (/=0) (iwn n f x v))
  where
    iwn :: Int -> (a -> a -> a) -> a -> IntMap a -> IntMap a
    iwn n' f' x' v'
      | n' >= 1 = iwn (n'-1) f' x' (M.insertWith f' n' x' v')
      | otherwise = v'

insertAllWithDefault :: (Eq a) => (a -> a) -> a -> Vector a -> Vector a
insertAllWithDefault f def (Vector n v) = Vector n (insertAll n v)
  where
    insertAll n' v'
      | n' >= 1 = insertAll (n'-1) (insertIfNotDefault n' v')
      | otherwise = v'
    insertIfNotDefault n' v' =
      let check = f $ M.findWithDefault def n' v'
      in if check /= def then M.insert n' check v' else M.delete n' v'


-- Combinations
-- Union
-- | Union with a binary function applied on intersections.
unionWith :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
unionWith f (Vector n v) (Vector m w)
  | n /= m = error "Uniting vectors of different dimension!"
  | otherwise = Vector n (M.unionWith f v w)

-- Intersection with a function applied.
intersectionWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
intersectionWith f (Vector n v) (Vector m w)
  | n /= m = error "Intersecting vectors of different dimension!"
  | otherwise = Vector n (M.intersectionWith f v w)




-- Map with regards to a key.
mapWithKey :: (Int -> a -> b) -> Vector a -> Vector b
mapWithKey f (Vector n v) = Vector n (M.mapWithKey f v)




-- Folds
-- | Fold the non-zero elements of the vector from the
-- highest to the lowest index.
foldrV :: (a -> b -> b) -> b -> Vector a -> b
foldrV f z (Vector _ v) = M.foldr f z v

-- | Fold with the key.
foldWithKey :: (Int -> a -> b -> b) -> b -> Vector a -> b
foldWithKey f z (Vector _ v) = M.foldWithKey f z v

unzip :: (Monoid a, Eq a) => Vector (a, a) -> (Vector a, Vector a)
unzip = foldWithKey
  (\i (x,y) (xs,ys) -> (insMonoid i x xs,insMonoid i y ys))
  (empty,empty)

-- | Sum all elements in the vector.
sum :: (Num a) => Vector a -> a
sum = foldr (+) 0

-- | The dot product.
dotP :: (Num a) => Vector a -> Vector a -> a
dotP v w = sum $ v .*. w





-- List conversions
-- | Converts from a list, throwing out all zeros.
fromList :: (Num a, Eq a) => [a] -> Vector a
fromList l = Vector (P.length l)
  (M.fromList [(i,x) | (i,x) <- zip [1..] l, x /= 0])

-- | Converts from a list of tuples @[(i,x)]@ to a vector with @x@ at @i@.
fromAssocList :: [(Int,a)] -> Vector a
fromAssocList l = Vector (L.length l) (M.fromList l)

-- | Make a vector of numbers a list of numbers, filling in zeros.
toList :: (Num a) => Vector a -> [a]
toList v = [ v ! i | i <- [1 .. dim v] ]

-- | Make a vector, filling in the default.
toListWithDefault :: a -> Vector a -> [a]
toListWithDefault def v = [ indexWithDefault v def i | i <- [1 .. dim v] ]

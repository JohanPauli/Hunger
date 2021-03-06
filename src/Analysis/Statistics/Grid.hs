{- |
  Typeclass-based statistics; uses some of the typeclasses from
  "Data.Grid.Node"
-}
module Analysis.Statistics.Grid where



-- Foldables:
import Data.Foldable as F

-- Applicative functors.
import Data.Functor ((<$>))

-- IntMaps:
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

-- Lists:
import Data.List (sortBy, groupBy)
import Data.Function (on)

-- Local:
import Data.Grid.Types
import Data.Grid.Topology
import Data.Grid.Node



-- Summary statistics.
-- | For counting elements of something.
count :: (Foldable f) => f a -> Int
count = F.foldl' (const . (+1)) 0

-- | Sum of some numeric attribute.
total :: (Foldable f, Num b) => (a -> b) -> f a -> b
total f = F.foldl' (\x e -> x+f e) 0

-- | Total generating capacity.
totalGenMax :: (Foldable f, Generator a) => f a -> CPower
totalGenMax = total genMax

-- | Total load.
totalLoad :: (Foldable f, Load a) => f a -> CPower
totalLoad = total loadPower



-- Detail statistics.
-- | Relevant bus data is ID, Complex voltage, generating power, load power.
type BusData = [(NodeID, CVoltage, CPower, CPower)]

-- | Bus data.
busData :: (Bus a, Generator b, Load c)
        => [a] -- ^ Grid buses.
        -> [b] -- ^ Generators attached to buses.
        -> [c] -- ^ Loads attached to buses.
        -> BusData
busData bs gs ls =
    fmap (\(a,(b,c,d)) -> (a,b,c,d)) $ M.toAscList
  $ M.intersectionWith addLoad lTots
  $ M.intersectionWith addGen gTots
  $ F.foldl' addBus M.empty bs
  where
    addBus :: (Bus a) => IntMap (CVoltage,CPower,CPower) -> a
                      -> IntMap (CVoltage,CPower,CPower)
    addBus resMap b = M.insert (nodeID b) (busVoltage b, 0, 0) resMap
    addGen (a,_,l) (_,g,_) = (a,g,l)
    addLoad (a,g,_) (_,_,l) = (a,g,l)

    -- Group generators and loads by bus id.
    grp :: (Noded a) => [a] -> [[a]]
    grp = groupBy ((==) `on` nodeID) . sortBy (compare `on` nodeID)

    -- Sum up a total and make it an IntMap with NodeIDs as keys.
    mkTots f = M.fromList . fmap (\es@(e:_) -> (nodeID e, total f es))

    -- The actual totals.
    gTots :: IntMap (CVoltage, CPower, CPower)
    gTots = (\a -> (0,a,0)) <$> (mkTots genPower . grp) gs
    lTots = (\a -> (0,0,a)) <$> (mkTots loadPower . grp) ls

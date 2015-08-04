{- |
  Data structure for representing the topological information in a
  power system.

  The topology of a power system is a graph. A graph is composed of Nodees
  and Edges connecting those Nodees. Properly, it is a directed multigraph.
-}
module Data.Grid.Topology
(
-- * Identifiers
  NodeID
, EdgeID

-- * Topological classes
, Topological (..)
, Noded (..)
, Edged (..)

-- * Topology type
, Topology (..)
, defaultTopology

-- * Node type
, Nodes
, Node (..)
, mkNodes

-- * Edge type
, Edges
, Edge (..)
, mkEdges

-- * Relational operations.
, joinEdges
, relabel
, shiftNodes
, nodeMap
) where



-- List processing:
import Data.List (sort,sortBy)

-- Fancy fmapping:
import Data.Functor ((<$>))

-- Functions on functions:
import Data.Function (on)

-- IntMap, for label transformations.
import Data.IntMap (IntMap,(!))
import qualified Data.IntMap as M



-- Utilities.
-- | ID synonym for a `Node` (makes it easier to see what is meant
-- by the number).
type NodeID = Int

-- | ID synonym for an `Edge` (makes it easier to see what is meant
-- by the number).
type EdgeID = Int



-- Topological properties.
-- | Something with a topology attached, e.g. a grid whose structure is
-- known in the form of buses and lines and their associations.
class Topological a where
  edges :: a -> Edges
  nodes :: a -> Nodes

-- | Something which is connected to a node in a topology.
class Noded a where
  nodeID :: a -> NodeID
  setNodeID :: NodeID -> a -> a

-- | Something which is connected to an edge in a topology.
class Edged a where
  edgeID :: a -> EdgeID



-- System types.
-- | A power system. A collection of Nodees and Edges with additional name
-- and power base information (for p.u. conversion).
data Topology = Topology Nodes Edges
  deriving (Eq, Show)

-- | Obviously (and tautologically) a topology is topological.
instance Topological Topology where
  nodes (Topology n _) = n
  edges (Topology _ e) = e

-- | A default system, for convenience.
defaultTopology :: Topology
defaultTopology =
  Topology (mkNodes 10) (mkEdges 10)



-- Node types.
-- | `Nodes` are a collection of `Node` items.
type Nodes = [Node]

-- | A `Node` is a uniquely identified item in a power system.
data Node = Node NodeID
  deriving (Eq,Ord,Show)

-- | A `Node` is node-connected.
instance Noded Node where
  nodeID (Node i) = i
  setNodeID j (Node _) = Node j

-- | Several, successively numbered (from 1), nodes.
mkNodes :: Int -> Nodes
mkNodes n = foldr (\n' l -> Node n':l) [] [1..n]



-- Edge types.
-- | `Edges` are a collection of `Edge` items.
type Edges = [Edge]

-- | An `Edge` is an item rightly identified by two `Node` identifiers, but
-- in this case an extra ID is given in case of parallel Edges.
data Edge = Edge EdgeID (NodeID, NodeID)
  deriving (Eq,Ord,Show)

-- | An `Edge` is edge-connected.
instance Edged Edge where
  edgeID (Edge i _) = i

-- | Several, successively numbered (from 1), nodes. Connections are
-- backwards and a loop is on node 1.
mkEdges :: Int -> Edges
mkEdges n = foldr addEdge [] [1..n]
  where
    addEdge 1 l = Edge 1 (1,1):l
    addEdge n' l = Edge n' (n',n'-1):l



-- Relational operations.
-- | The relational JOIN operation, makes a tuple of (the joined type, its
-- associated edge).
--
-- For operations on elements which require its topological information.
joinEdges :: (Topological a, Edged b) => a -> [b] -> [(b,Edge)]
joinEdges top ecs = [(e,find e es) | e <- ecs]
  where
    es = edges top
    find :: (Edged b) => b -> [Edge] -> Edge
    find ec (Edge i (a,b):es') =
      if edgeID ec == i then Edge i (a,b) else find ec es'
    find _ _ = error "Something wrong with the topology"


-- | Relabels a topology's nodes by provided node categories.
--
-- This might not be a good idea.
relabel :: (Ord a)
        => IntMap a -- ^ Categories to sort by.
        -> Nodes -- ^ Nodes to sort.
        -> Edges -- ^ Edges must reflect node labels.
        -> (IntMap NodeID, Edges) -- ^ (ID shift map, New Edges).
relabel is ns es = (newIs, newEs)
  where
    nIDs = sort $ fmap nodeID ns
    newIs = M.fromList $ zip sorted nIDs
    sorted = fmap fst $ sortBy (compare `on` snd) $ M.toList is
    chEdge (Edge n (i,j)) = Edge n (newIs!i, newIs!j)
    newEs = fmap chEdge es

-- | Shift the node labels of node-connected elements.
--
-- Assumes that the map is total (the ID of every shifted node is in its
-- keyset). This isn't guaranteed, so this should probably be changed.
shiftNodes :: (Noded a) => IntMap NodeID -> [a] -> [a]
shiftNodes shifts = fmap (\nc -> setNodeID (shifts!nodeID nc) nc)

-- | Make an IntMap representing a topological sort of something
-- node-connected; sorted by and Ord(ering).
nodeMap :: (Ord a, Noded a) => [a] -> IntMap Int
nodeMap classes = M.fromList $ zip sorted [1..]
  where
    sorted = nodeID <$> sort classes

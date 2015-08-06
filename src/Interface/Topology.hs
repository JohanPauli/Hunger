{- |
  Data structure for representing the topological information in a
  power system.

  The topology of a power system is a graph. A graph is composed of Nodees
  and Edges connecting those Nodees. Properly, it is a directed multigraph.
-}
module Interface.Topology
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

-- * Operations on Topologies
, joinEdges
, nodeMap
) where



-- List processing:
import Data.List (sort)

-- Fancy fmapping:
import Data.Functor ((<$>))

-- IntMapping:
import Data.IntMap (IntMap)
import qualified Data.IntMap as I



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



-- Operations having to do with topologies
-- | The relational JOIN operation, makes a tuple of (the joined type, its
-- associated edge).
--
-- For operations on elements which require its topological information.
joinEdges :: (Topological a, Edged b) => a -> [b] -> [(Edge,b)]
joinEdges top ecs = fmap snd $ I.toList $ joinMaps es ec
  where
    es = I.fromList [(edgeID e,e) | e <- edges top]
    ec = I.fromList [(edgeID e,e) | e <- ecs]

-- | A relational JOIN operation on Int-indexed maps.
--
-- Assumes (a big assumption) that both lists are of equal size.
joinMaps :: IntMap a -> IntMap b -> IntMap (a,b)
joinMaps = I.intersectionWith (,)

-- | Make an IntMap representing a topological sort of something
-- node-connected; sorted by an Ord(ering).
nodeMap :: (Ord a, Noded a) => [a] -> IntMap Int
nodeMap classes = I.fromList $ zip sorted [1..]
  where
    sorted = nodeID <$> sort classes

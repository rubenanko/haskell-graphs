{- |
  Module      : Graph
  Description : A module for directed (by default) graphs
  Copyright   : ???
  Maintainer  : ???
-}
module Graph where

import Prelude hiding (reverse)
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- | * Types

-- | A directed edge is merely a tuple (origin, destination)
type Edge v -- ^ Type for vertex
  = (v, v)

-- | The reverse edge of input edge, i.e. swapping origin and destination
reverseEdge :: Edge v -> Edge v
reverseEdge (v, v') = (v', v)

-- | A directed graph
-- | Chosen implementation is a map:
-- |  - whose keys are all vertices
-- |  - and values are the list of successors (repeated by the number of actual edges)
-- | **It is assumed that any value that is not a key is not a vertex.**
-- | **Hence zero-degree vertices are keys in the map whose value is an empty list.**
newtype Graph v -- ^ Type for vertex
  = Graph { neighborsMap :: Map v [v] }
  deriving Show

-- | Equality test on graphs
instance Eq v => Eq (Graph v) where
  (Graph nsMap) == (Graph nsMap') =
    ((length ls) == (length ls')) && (and $ zipWith (\(k, ns) (k', ns') -> (k == k') && (ns `hasSameElementsAs` ns')) ls ls')
      where ls  = Map.toList nsMap
            ls' = Map.toList nsMap'

-- | Do both lists have the same elements with the same count ?
hasSameElementsAs :: Eq a => [a] -> [a] -> Bool
l `hasSameElementsAs` l' = (null $ l' \\ l) && (null $ l \\ l')

-- !!WARNING!!
-- Some operations on 'Map k a' require type 'k' to be an instance of 'Ord'.
-- Hence constraint 'Ord v =>' will be required on many functions.

-- | * Build functions

-- | The empty graph
empty :: Graph v
empty = undefined

-- | Build a graph with input set of vertices and no edge
fromVertices :: Set v -> Graph v
fromVertices _ = undefined

-- | Build a graph with input set of edges 
fromEdges :: [Edge v] -> Graph v
fromEdges _ = undefined

-- | Build a strict version of input graph (i.e. with neither loops nor parallel edges)
toStrict :: Graph v -> Graph v
toStrict _ = undefined

-- | Build an "undirected" (i.e. symmetric) version input graph by adding reverse edges when needed
toUndirected :: Graph v -> Graph v
toUndirected _ = undefined

-- | Build a simple version of input graph (i.e. undirected and strict)
toSimple :: Graph v -> Graph v
toSimple = undefined

-- | * Query functions

-- | The set of vertices
vertices :: Graph v -> Set v
vertices = Map.keysSet . neighborsMap

-- | The list of edges
edges :: Graph v -> [Edge v]
edges _ = undefined

-- | The list of successors
-- | Return 'Nothing' if input is not an actual vertex
successorsListOf :: Graph v -> v -> Maybe [v]
_ `successorsListOf` _ = undefined

-- | The list of predecessors
-- | Return 'Nothing' if input is not an actual vertex
predecessorsListOf :: Graph v -> v -> Maybe [v]
_ `predecessorsListOf` _ = undefined

-- | The outer degree (i.e. number of outgoing edges)
-- | Return 'Nothing' if input is not an actual vertex
outerDegreeOf :: Graph v -> v -> Maybe Int
_ `outerDegreeOf` _ = undefined

-- | The inner degree (i.e. number of incoming edges)
-- | Return `Nothing` if input is not an actual vertex
innerDegreeOf :: Graph v -> v -> Maybe Int
_ `innerDegreeOf` _ = undefined

-- | Is input edge sequence an actual path on input graph ?
-- | Return True if input list is empty
isPathOn :: [Edge v] -> Graph v -> Bool
_ `isPathOn` _ = undefined

-- | Attempts to find an actual path in input graph between input vertices
-- | Return `Nothing` if at least one end is not an actual vertex
-- | Return `Nothing` if path does not exist
-- | Return empty list if both ends are the same
findPath :: Graph v -> (v, v) -> Maybe [Edge v]
_ `findPath` _ = undefined

-- | Is there an actual path in input graph between input vertices
-- | Return False if at least one end is not an actual vertex
hasPath :: Graph v -> (v, v) -> Bool
_ `hasPath` _ = undefined

-- | Is the input graph strongly connected ?
isConnected :: Graph v -> Bool
isConnected _ = undefined

-- | Has the input graph a circuit ?
hasCircuit :: Graph v -> Bool
hasCircuit _ = undefined

-- | Is the input graph strict (i.e. has neither loops nor parallel edges) ?
isStrict :: Graph v -> Bool
isStrict _ = undefined

-- | Is the input graph "undirected" (i.e. symmetric for all edges) ?
isUndirected :: Graph v -> Bool
isUndirected _ = undefined

-- | Is the input graph simple (i.e. undirected and strict) ?
isSimple :: Graph v -> Bool
isSimple _ = undefined

-- | * Vertex operations

-- | Build a new graph from input graph with a new vertex
-- | Return input graph if input is already an actual vertex of graph
(+.) :: Graph v -> v -> Graph v
_ +. _ = undefined

-- | Build a new graph from input graph without input vertex (and all edges linked to it)
-- | Return input graph if input is not an actual vertex of graph
(-.) :: Graph v -> v -> Graph v
_ -. _ = undefined

-- | * Edge operations

-- | Build a new graph from input graph with a new edge (add its ends if necessary)
(+|) :: Graph v -> Edge v -> Graph v
_ +| _ = undefined

-- | Build a new graph from input graph without input edge (does NOT remove ends)
-- | Return input graph if input is not an actual edge of graph
(-|) :: Graph v -> Edge v -> Graph v
_ -| _ = undefined

-- | Build a new graph from input graph with all edges reversed
-- | Return input graph if undirected
reverse :: Graph v -> Graph v
reverse _ = undefined

-- | Build a new graph with same vertices as input graph but no edge
withoutEdge :: Graph v -> Graph v
withoutEdge _ = undefined

-- | Build a new graph with same vertices as input graph and all possibles edges (a.k.a a complete graph)
withAllEdges :: Graph v -> Graph v
withAllEdges _ = undefined

-- | * Topological sort

-- | Computes a topological sort of graph (if possible)
-- | Return `Nothing` if a topological cannot be computed
topologicalSort :: Graph v -> Maybe [v]
topologicalSort _ = undefined

-- | * DOT representation  

-- | Computes the DOT representation string of graph
-- | First parameter is the graph name
toDOTString :: (Ord v, Show v) => String -> Graph v -> String
toDOTString name g@(Graph nsMap) = "digraph " ++ name ++ "\n"
  ++ "  {\n" ++ verticesString ++ edgesString ++ "  }\n"
  where verticesString   = concat $ Map.mapWithKey vertexString vertexNumbering
        vertexString v i = "    " ++ (show i) ++ " [label = \"" ++ (show v) ++ "\"]\n"
        edgesString      = concat $ Map.mapWithKey (\v ns -> concatMap (edgeString v) ns) nsMap
        edgeString v n   = "    " ++ (show $ vertexNumbering ! v) ++ " -> " ++ (show $ vertexNumbering ! n) ++ "\n"
        vertexNumbering  = Map.fromList $ zip (Set.toList $ vertices g) [1..]
        
-- | Writes the DOT representation of a graph into a file
-- | First parameter is the graph name
-- | Output file name is graph name with extension .dot
toDOTFile :: (Ord v, Show v) => String -> Graph v -> IO ()
toDOTFile name = (writeFile $ name ++ ".dot") . (toDOTString name)



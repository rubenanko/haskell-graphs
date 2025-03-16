{- |
  Module : GraphSpec
  Description : A specification for Graph type
  Copyright   : (c) Romain DUJOL, 2025
  Maintaineer : romain.dujol@cyu.fr
-}
module GraphSpec(spec, IntVertex(..), IntVertexSet(..), IntEdge(..), IntEdgeList(..), IntGraph(..), allPossibleNonLoopEdges, implies, haveBothEndsIn) where

import Prelude hiding (reverse)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.List (elemIndex, isSubsequenceOf)
import qualified Data.List as List (delete, sort)
import Data.Set (Set, member, notMember, union, isSubsetOf)
import qualified Data.Set as Set (null, fromList, toList, singleton, delete)
import qualified Data.Map as Map (fromList)

import Graph

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- | * Generators

-- | The generator for integer-valued vertices
newtype IntVertex = IntVertex { vertex :: Int }
instance Show IntVertex where
  show = show . vertex
instance Arbitrary IntVertex where
  arbitrary = sized $ \n ->
    do
      v <- chooseInt(0, n)
      return $ IntVertex v
-- | The generator for non-empty sets of integer-valued vertices
newtype IntVertexSet = IntVertexSet { vertexSet :: Set Int }
instance Show IntVertexSet where
  show = show . vertexSet
instance Arbitrary IntVertexSet where
  arbitrary = do
    vs <- listOf1 $ fmap vertex arbitrary
    return $ IntVertexSet $ Set.fromList $ vs

-- | The generator for edges with integer vertices
newtype IntEdge = IntEdge { edge :: Edge Int }
instance Show IntEdge where
  show = show . edge
instance Arbitrary IntEdge where
  arbitrary = do
    (iV, iV') <- arbitrary
    return $ IntEdge $ (vertex iV, vertex iV')
-- | The generator for list of at least one edge with integer vertices
newtype IntEdgeList = IntEdgeList { edgeList :: [Edge Int] }
instance Show IntEdgeList where
  show = show . edgeList
instance Arbitrary IntEdgeList where
  arbitrary = do
    es <- listOf1 $ fmap edge arbitrary
    return $ IntEdgeList es

-- | The generator for graphs with at least one integer vertex
newtype IntGraph = IntGraph { graph :: Graph Int }
instance Show IntGraph where
  show = show . graph
instance Arbitrary IntGraph where
  arbitrary = do
    vs <- fmap (Set.toList . vertexSet) arbitrary
    ns <- vectorOf (length vs) $ listOf (elements vs)
    return $ IntGraph $ Graph $ Map.fromList $ zip vs ns

-- | The generator for graphs with at least one integer vertex and a vertex
newtype IntGraphAndActualVertex = IntGraphAndActualVertex { graphAndActualVertex :: (Graph Int, Int) }
instance Show IntGraphAndActualVertex where
  show = show . graphAndActualVertex
instance Arbitrary IntGraphAndActualVertex where
  arbitrary = do
    g <- fmap graph arbitrary
    v <- elements $ Set.toList $ vertices g
    return $ IntGraphAndActualVertex (g, v)

-- | The generator for graphs with at least one integer vertex and three vertices
newtype IntGraphAnd3ActualVertices = IntGraphAnd3ActualVertices { graphAnd3ActualVertices :: (Graph Int, Int, Int, Int) }
instance Show IntGraphAnd3ActualVertices where
  show = show . graphAnd3ActualVertices
instance Arbitrary IntGraphAnd3ActualVertices where
  arbitrary = do
    g  <- fmap graph arbitrary
    vs <- vectorOf 3 $ elements $ Set.toList $ vertices g
    return $ IntGraphAnd3ActualVertices (g, vs !! 0, vs !! 1, vs !! 2)

-- | * Some useful operations

implies :: Bool -> Bool -> Bool
p `implies` q = (not p) || q

haveBothEndsIn :: Ord v => (v, v) -> Set v -> Bool
(v, v') `haveBothEndsIn` vs = (v `member` vs) && (v' `member` vs)

allPossibleNonLoopEdges :: Eq v => Set v -> [Edge v]
allPossibleNonLoopEdges vs = [(v, v') | v <- vsL, v' <- vsL, v /= v]
  where vsL = Set.toList vs

isTopologicallySortedOn :: Eq v => Maybe [v] -> Graph v -> Bool
Nothing `isTopologicallySortedOn` _ = False
Just vs `isTopologicallySortedOn` g = all (\(v, v') -> (v `elemIndex` vs < v' `elemIndex` vs)) (edges g)

-- | * Tests

spec :: Spec
spec = do
  describe "empty" $ do
    it "must have no vertex" $
      (Set.null $ vertices empty)
    it "must have no edge"   $
      (    null $    edges empty)

  describe "(==)" $ do
    prop "must be reflexive" $
      \(IntGraph g) -> g `shouldBe` g
    prop "must yield same result as only comparing vertices and edges" $
      \(IntGraph g) (IntGraph g') -> ((vertices g == vertices g') && (List.sort $ edges g) == (List.sort $ edges g')) `shouldBe` (g == g')

  describe "fromVertices" $ do
    prop "must have correct set of vertices" $
      \(IntVertexSet vs) -> (vertices $ fromVertices vs) `shouldBe` vs
  describe "fromEdges" $ do
    prop "must have correct list of edges" $
      \(IntEdgeList  es) -> (   edges $ fromEdges    es) `shouldMatchList` es

  describe "edges" $ do
    prop "must yield edges whose ends are in the set of vertices" $
      \(IntGraph g) -> all (`haveBothEndsIn` (vertices g)) $ edges g

  describe "successorsListOf" $ do
    prop "must yield Nothing if and only input is not an actual vertex" $
      \(IntGraph g) (IntVertex v) -> (isNothing $ g   `successorsListOf` v) `shouldBe` (v `notMember` (vertices g))
    prop "must yield only vertices from set of vertices of graph" $
      \(IntGraphAndActualVertex (g, v)) -> (Set.fromList $ fromJust $ g   `successorsListOf` v) `isSubsetOf` (vertices g)
  describe "predecessorsListOf" $ do
    prop "must yield Nothing if and only input is not an actual vertex" $
      \(IntGraph g) (IntVertex v) -> (isNothing $ g `predecessorsListOf` v) `shouldBe` (v `notMember` (vertices g))
    prop "must yield only vertices from set of vertices of graph" $
      \(IntGraphAndActualVertex (g, v)) -> (Set.fromList $ fromJust $ g `predecessorsListOf` v) `isSubsetOf` (vertices g)
  describe "outerDegreeOf" $ do
    prop "must yield the number of successors" $
      \(IntGraph g) (IntVertex v) -> (fmap length $ g   `successorsListOf` v) `shouldBe` (g `outerDegreeOf` v)
    prop "must validate the handshake lemma" $
      \(IntGraph g) -> (sum $ map (fromJust . (g `outerDegreeOf`)) $ Set.toList $ vertices g) `shouldBe` (length $ edges g)
  describe "innerDegreeOf" $ do
    prop "must yield the number of predecessors" $
      \(IntGraph g) (IntVertex v) -> (fmap length $ g `predecessorsListOf` v) `shouldBe` (g `innerDegreeOf` v)
    prop "must validate the handshake lemma" $
      \(IntGraph g) -> (sum $ map (fromJust . (g `innerDegreeOf`)) $ Set.toList $ vertices g) `shouldBe` (length $ edges g)

  describe "(+.)" $ do
    prop "must update the set of vertices accordingly" $
      \(IntGraph g) (IntVertex v) -> (vertices $ g +. v) `shouldBe` (vertices g `union` Set.singleton v)
    prop "must leave the list of edges unchanged" $
      \(IntGraph g) (IntVertex v) -> (   edges $ g +. v) `shouldMatchList` (edges g)
  describe "(-.)" $ do
    prop "must update the set of vertices accordingly" $
      \(IntGraph g) (IntVertex v) -> (vertices $ g -. v) `shouldBe` (Set.delete v $ vertices g)
  describe "(+|)" $ do
    prop "must update the set of vertices accordingly" $
      \(IntGraph g) (IntEdge e@(v, v')) -> (vertices $ g +| e) `shouldBe` (vertices g `union` Set.fromList [v, v'])
    prop "must update the list of edges    accordingly" $
      \(IntGraph g) (IntEdge e) -> (edges $ g +| e) `shouldMatchList` (e : edges g)
  describe "(-|)" $ do
    prop "must leave the set of vertices unchanged" $
      \(IntGraph g) (IntEdge e) -> (vertices $ g -| e) `shouldBe` (vertices g)
    prop "must update the list of edges accordingly" $
      \(IntGraph g) (IntEdge e) -> (edges $ g -| e) `shouldMatchList` (List.delete e $ edges g)

  describe "withoutEdge" $ do
    prop "must leave the set of vertices unchanged" $
      \(IntGraph g) -> (vertices $ withoutEdge g) `shouldBe` (vertices g)
    prop "must have an empty list of edges" $
      \(IntGraph g) -> (   edges $ withoutEdge g) `shouldSatisfy` null

  describe "withAllEdges" $ do
    prop "must leave the set of vertices unchanged" $
      \(IntGraph g) -> (vertices $ withAllEdges g) `shouldBe` (vertices g)
    prop "must have all possible edges" $
      \(IntGraph g) -> (List.sort $ allPossibleNonLoopEdges $ vertices g) `isSubsequenceOf` (List.sort $ edges g)
    prop "must yield a simple graph" $
      \(IntGraph g) -> (withAllEdges g) `shouldSatisfy` isSimple
    prop "must yield a connected graph" $
      \(IntGraph g) -> (withAllEdges g) `shouldSatisfy` isConnected

  describe "reverse" $ do
    prop "must yield the input graph when applied twice" $
      \(IntGraph g) -> (reverse $ reverse g) `shouldBe` g
    prop "must leave the set of vertices unchanged" $
      \(IntGraph g) -> (vertices $ reverse g) `shouldBe` (vertices g)
    prop "must update the list of edges accordingly" $
      \(IntGraph g) -> (   edges $ reverse g) `shouldMatchList` (map (\(v, v') -> (v', v)) $ edges g)
    prop "must exchanges successors and predecessors" $
      \(IntGraph g) -> let sortM = fmap List.sort in
                         all (\v -> (sortM $ (reverse g)   `successorsListOf` v) == (sortM $ g `predecessorsListOf` v)
                                 && (sortM $ (reverse g) `predecessorsListOf` v) == (sortM $ g   `successorsListOf` v)) (Set.toList $ vertices g)
    prop "must keep strong connectivity unchanged" $
      \(IntGraph g) -> (isConnected $ reverse g) `shouldBe` (isConnected g)
    prop "must keep strictness unchanged" $
      \(IntGraph g) -> (isStrict    $ reverse g) `shouldBe` (isStrict    g)
    prop "must yield input graph if undirected" $
      \(IntGraph g) -> let g' = toUndirected g in (reverse g') `shouldBe` g'
    prop "must be compatible with adding a vertex" $
      \(IntGraph g) (IntVertex v) -> (reverse $ g +. v) `shouldBe` ((reverse g) +.              v )
    prop "must be compatible with removing a vertex" $
      \(IntGraph g) (IntVertex v) -> (reverse $ g -. v) `shouldBe` ((reverse g) -.              v )
    prop "must, when edge is   added beforehand, yield same result as   adding reverse edge afterwards" $
      \(IntGraph g) (IntEdge   e) -> (reverse $ g +| e) `shouldBe` ((reverse g) +| (reverseEdge e))
    prop "must, when edge is removed beforehand, yield same result as removing reverse edge afterwards" $
      \(IntGraph g) (IntEdge   e) -> (reverse $ g -| e) `shouldBe` ((reverse g) -| (reverseEdge e))

  describe "isSimple" $ do
    prop "must yield same result as being undirected and strict" $ do
      \(IntGraph g) -> (isStrict g) && (isUndirected g) `shouldBe` (isSimple g)
  describe "toStrict" $ do
    prop "must yield a strict graph (as per isStrict)" $
      \(IntGraph g) -> toStrict     g `shouldSatisfy` isStrict
  describe "toUndirected" $ do
    prop "must yield a graph with no less edges than input" $
      \(IntGraph g) -> (List.sort $ edges g) `isSubsequenceOf` (List.sort $ edges $ toUndirected g)
    prop "must yield an undirected graph (as per isUndirected)" $
      \(IntGraph g) -> toUndirected g `shouldSatisfy` isUndirected
  describe "toSimple" $ do
    prop "must yield a simple graph (as per isSimple)" $
      \(IntGraph g) -> toSimple     g `shouldSatisfy` isSimple
    prop "must yield the same result as toStrict . toUndirected" $
      \(IntGraph g) -> (toStrict $ toUndirected g) `shouldBe` (toSimple g)
    prop "must yield the same result as toUndirected . toStrict" $
      \(IntGraph g) -> (toUndirected $ toStrict g) `shouldBe` (toSimple g)

  describe "findPath" $ do
    prop "must yield Nothing if any end is not an actual vertex" $
      \(IntGraph g) (IntEdge e) -> (not $ e `haveBothEndsIn` (vertices g)) ==> (g `findPath` e) `shouldSatisfy` isNothing
    modifyMaxDiscardRatio (* 2) $ prop "must yield an actual result for the ends of an actual edge" $
      \(IntGraph g) (IntEdge e) -> (e `elem` edges g) ==> (g `findPath` e) `shouldSatisfy` isJust
    prop "must yield an actual path (as per isPathOn)" $
      \(IntGraph g) (IntEdge e) -> (e `haveBothEndsIn` (vertices g)) ==> maybe True (`isPathOn` g) $ g `findPath` e
  describe "hasPath" $ do
    prop "must be reflexive" $
      \(IntGraphAndActualVertex (g, v)) -> (v, v) `shouldSatisfy` (g `hasPath`)
    prop "must be transitive" $
      \(IntGraphAnd3ActualVertices (g, v, v', v'')) -> ((g `hasPath` (v, v')) && (g `hasPath` (v', v''))) `implies` (g `hasPath` (v', v''))

  describe "isConnected" $ do
    prop "must yield a graph for which every path exists" $
      \(IntGraph g) -> all (g `hasPath`) $ allPossibleNonLoopEdges $ vertices g

  describe "topologicalSort" $ do
    prop "must yield Nothing if and only if graph has no circuit" $
      \(IntGraph g) -> (isNothing $ topologicalSort g) `shouldBe` (hasCircuit g)
    modifyMaxSuccess (const 50) $ modifyMaxDiscardRatio (const 100000) $ prop "must be a permutation of the set of vertices" $
      \(IntGraph g) -> (not $ hasCircuit g) ==> (fmap List.sort $ topologicalSort g) `shouldBe` (Just $ List.sort $ Set.toList $ vertices g)
    modifyMaxSuccess (const 50) $ modifyMaxDiscardRatio (const 100000) $ prop "must yield a topologically sorted sequence in graph" $
      \(IntGraph g) -> (not $ hasCircuit g) ==> (topologicalSort g) `shouldSatisfy` (`isTopologicallySortedOn` g)

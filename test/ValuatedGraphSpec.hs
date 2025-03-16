module ValuatedGraphSpec where

import Prelude hiding (reverse)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.List (isSubsequenceOf)
import qualified Data.List as List (delete, sort)
import Data.Set (notMember, isSubsetOf, union)
import qualified Data.Set as Set (empty, singleton, fromList, toList, null, delete, findMin)
import Data.Map (Map, (!?))
import qualified Data.Map as Map (fromList, keysSet)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Graph (Edge)
import qualified Graph as Graph
import ValuatedGraph

import GraphSpec

-- | The generator for positive values for edges
newtype PosIntValue = PosIntValue { value :: Int }
instance Show PosIntValue where
  show = show . value
instance Arbitrary PosIntValue where
  arbitrary = sized $ \n ->
    do
      x <- chooseInt(0, n `div` 2)
      return $ PosIntValue x

-- | The generator for a positve integer-valued valuation map
newtype IntValuationMap = IntValuationMap { valuationMap :: Map (Edge Int) Int }
instance Show IntValuationMap where
  show = show . valuationMap
instance Arbitrary IntValuationMap where
  arbitrary = do
    es <- fmap edgeList arbitrary
    xs <- vectorOf (length es) $ fmap value arbitrary
    return $ IntValuationMap $ Map.fromList $ zip es xs

-- | The generator for positive integer-valuated graphs with at least one integer vertex
newtype IntValuatedGraph = IntValuatedGraph { valuatedGraph :: ValuatedGraph Int Int }
instance Show IntValuatedGraph where
  show = show . valuatedGraph
instance Arbitrary IntValuatedGraph where
  arbitrary = do
    vs <- fmap (Set.toList . vertexSet) $ arbitrary
    ns <- vectorOf (length vs) $ listOf $ liftA2 (,) (elements vs) (fmap value $ arbitrary)
    return $ IntValuatedGraph $ ValuatedGraph $ Map.fromList $ zip vs ns

-- | The generator for positive integer-valuated graphs with at least one integer vertex and a vertex
newtype IntValuatedGraphAndActualVertex = IntValuatedGraphAndActualVertex { valuatedGraphAndActualVertex :: (ValuatedGraph Int Int, Int) }
instance Show IntValuatedGraphAndActualVertex where
  show = show . valuatedGraphAndActualVertex
instance Arbitrary IntValuatedGraphAndActualVertex where
  arbitrary = do
    vg <- fmap valuatedGraph arbitrary
    v  <- elements $ Set.toList $ vertices vg
    return $ IntValuatedGraphAndActualVertex (vg, v)

-- | The generator for positive integer-valuated graphs with at least one integer vertex and two vertices
newtype IntValuatedGraphAnd2ActualVertices = IntValuatedGraphAnd2ActualVertices { valuatedGraphAnd2ActualVertices :: (ValuatedGraph Int Int, Int, Int) }
instance Show IntValuatedGraphAnd2ActualVertices where
  show = show . valuatedGraphAnd2ActualVertices
instance Arbitrary IntValuatedGraphAnd2ActualVertices where
  arbitrary = do
    vg <- fmap valuatedGraph arbitrary
    vs <- vectorOf 2 $ elements $ Set.toList $ vertices vg
    return $ IntValuatedGraphAnd2ActualVertices (vg, vs !! 0, vs !! 1)

spec :: Spec
spec = do
  describe "empty" $ do
    it "must have no vertex" $
      (Set.null $ vertices empty)
    it "must have no edge"   $
      (    null $    edges empty)

  describe "(==)" $ do
    prop "must be reflexive" $
      \(IntValuatedGraph vg) -> vg `shouldBe` vg

  describe "fromVertices" $ do
    prop "must have correct set of vertices" $
      \(IntVertexSet vs) -> (vertices $ fromVertices vs) `shouldBe` vs
  describe "fromValuationMap" $ do
    prop "must have correct set of edges" $ 
      \(IntValuationMap vm) -> (edges $ fromValuationMap vm) `shouldMatchList` (Set.toList $ Map.keysSet vm)
    prop "must have matching valuations" $
      \(IntValuationMap vm) -> let vg = fromValuationMap vm in
                                 all (\e -> (vg `valueAt` e) == (fmap Set.singleton $ vm !? e)) $ edges vg

  describe "edges" $ do
    prop "must yield edges whose ends are in the set of vertices" $
      \(IntValuatedGraph vg) -> all (`haveBothEndsIn` (vertices vg)) $ edges vg

  describe "successorsListOf" $ do
    prop "must yield Nothing if and only input is not an actual vertex" $
      \(IntValuatedGraph vg)       (IntVertex v) -> (isNothing $ vg   `successorsListOf` v) `shouldBe` (v `notMember` (vertices vg))
    prop "must yield only vertices from set of vertices of graph" $
      \(IntValuatedGraphAndActualVertex (vg, v)) -> (Set.fromList $ map fst $ fromJust $ vg   `successorsListOf` v) `isSubsetOf` (vertices vg)
  describe "predecessorsListOf" $ do
    prop "must yield Nothing if and only input is not an actual vertex" $
      \(IntValuatedGraph vg)       (IntVertex v) -> (isNothing $ vg `predecessorsListOf` v) `shouldBe` (v `notMember` (vertices vg))
    prop "must yield only vertices from set of vertices of graph" $
      \(IntValuatedGraphAndActualVertex (vg, v)) -> (Set.fromList $ map fst $ fromJust $ vg `predecessorsListOf` v) `isSubsetOf` (vertices vg)

  describe "valueAt" $ do
    prop "must yield Nothing if and only if one and of input is not an actual vertex" $
      \(IntValuatedGraph vg) (IntEdge e) -> (isNothing $ vg `valueAt` e) `shouldBe` (not $ e `haveBothEndsIn` (vertices vg))
    prop "must yield null set if both ends are actual vertices but input is not an actual edge" $
      \(IntValuatedGraph vg) (IntEdge e) -> ((vg `valueAt` e) == (Just Set.empty)) `shouldBe` ((e `haveBothEndsIn` (vertices vg)) && (e `notElem` edges vg))

  describe "toUnvaluated" $ do
    prop "must have same set of vertices as input" $
      \(IntValuatedGraph vg) -> (Graph.vertices $ toUnvaluated vg) `shouldBe` (vertices vg)
    prop "must have same list of edges as input" $
      \(IntValuatedGraph vg) -> (Graph.edges    $ toUnvaluated vg) `shouldBe` (   edges vg)

  describe "toUndirected" $ do
    prop "must yield a graph with no less edges than input" $
      \(IntValuatedGraph vg) -> (List.sort $ edges vg) `isSubsequenceOf` (List.sort $ edges $ toUndirected vg)
    prop "must yield an undirected graph (as per isUndirected)" $
      \(IntValuatedGraph vg) -> toUndirected vg `shouldSatisfy` isUndirected

  describe "(+.)" $ do
    prop "must update the set of vertices accordingly" $
      \(IntValuatedGraph vg) (IntVertex v) -> (vertices $ vg +. v) `shouldBe` (vertices vg `union` Set.singleton v)
    prop "must leave the list of edges unchanged" $
      \(IntValuatedGraph vg) (IntVertex v) -> (   edges $ vg +. v) `shouldMatchList` (edges vg)
    prop "must commute with toUnvaluated" $
      \(IntValuatedGraph vg) (IntVertex v) -> (toUnvaluated $ vg +. v) `shouldBe` ((toUnvaluated vg) Graph.+. v)
  describe "(-.)" $ do
    prop "must update the set of vertices accordingly" $
      \(IntValuatedGraph vg) (IntVertex v) -> (vertices $ vg -. v) `shouldBe` (Set.delete v $ vertices vg)
    prop "must commute with toUnvaluated" $
      \(IntValuatedGraph vg) (IntVertex v) -> (toUnvaluated $ vg -. v) `shouldBe` ((toUnvaluated vg) Graph.-. v)
  describe "(+|)" $ do
    prop "must update the set of vertices accordingly" $
      \(IntValuatedGraph vg) (IntEdge e@(v, v')) (PosIntValue x) -> (vertices $ vg +| (e, x)) `shouldBe` (vertices vg `union` Set.fromList [v, v'])
    prop "must update the list of edges    accordingly" $
      \(IntValuatedGraph vg) (IntEdge e        ) (PosIntValue x) -> (   edges $ vg +| (e, x)) `shouldMatchList` (e : edges vg)
    prop "must commute with toUnvaluated" $
      \(IntValuatedGraph vg) (IntEdge e        ) (PosIntValue x) -> (toUnvaluated $ vg +| (e, x)) `shouldBe` ((toUnvaluated vg) Graph.+| e)
  describe "(-|)" $ do
    prop "must leave the list of vertices unchanged" $
      \(IntValuatedGraph vg) (IntEdge e        ) (PosIntValue x) -> (vertices $ vg -| (e, x)) `shouldBe` (vertices vg)

  describe "reverse" $ do
    prop "must yield the input graph when applied twice" $
      \(IntValuatedGraph vg) -> (reverse $ reverse vg) `shouldBe` vg
    prop "must leave the set of vertices unchanged" $
      \(IntValuatedGraph vg) -> (vertices $ reverse vg) `shouldBe` (vertices vg)
    prop "must update the list of edges accordingly" $
      \(IntValuatedGraph vg) -> (   edges $ reverse vg) `shouldMatchList` (map (\(v, v') -> (v', v)) $ edges vg)
    prop "must commute with toUnvaluated" $
      \(IntValuatedGraph vg) -> (toUnvaluated $ reverse vg) `shouldBe` (Graph.reverse $ toUnvaluated vg)
    prop "must update the valuation" $
      \(IntValuatedGraph vg) -> all (\e -> ((reverse vg) `valueAt` (Graph.reverseEdge e)) == (vg `valueAt` e)) $ allPossibleNonLoopEdges $ vertices vg

  describe "shortestPath" $ do
    prop "must yield Nothing if one and of input is not an actual vertex" $
      \(IntValuatedGraph vg) (IntEdge e) -> (not $ e `haveBothEndsIn` (vertices vg)) `implies` (isNothing $ vg `shortestPath` e)
    prop "must yield an actual path in the graph it is exists" $
      \(IntValuatedGraphAnd2ActualVertices (vg, v, v')) -> let sp = vg `shortestPath` (v, v')
                                                               p  = fmap (map fst) sp in
                                                             (isJust sp) `implies` ((isJust p) && fromJust p `Graph.isPathOn` (toUnvaluated vg))
    prop "must yield an actual result if a path exists" $
      \(IntValuatedGraph vg) (IntEdge e) -> (toUnvaluated vg `Graph.hasPath` e) `implies` (isJust $ vg `shortestPath` e)
    modifyMaxDiscardRatio (const 1000) $ prop "must yield a path shorter than any path found with findPath" $
      \(IntValuatedGraph vg) (IntEdge e) -> let g = toUnvaluated vg
                                                p = g `Graph.findPath` e in
                                              (isJust p) ==> let sp = fromJust $ vg `shortestPath` e
                                                                 pv = sum $ map (\e' -> Set.findMin $ fromJust $ vg `valueAt` e') $ fromJust p in
                                                               (sum $ map snd sp) `shouldSatisfy` (<= pv)


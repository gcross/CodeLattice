-- @+leo-ver=4-thin
-- @+node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100312220352.1855:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100312220352.1855:<< Language extensions >>
-- @nl

module CodeLattice where

-- @<< Import needed modules >>
-- @+node:gcross.20100302164430.1307:<< Import needed modules >>
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Char
import Data.Either
import Data.Eq.Approximate
import Data.Function
import qualified Data.IntSet as IntSet
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
-- @-node:gcross.20100302164430.1307:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100302164430.1234:Types
-- @+node:gcross.20100717003017.2415:ApproximateDouble
data MyTolerance
instance AbsoluteTolerance MyTolerance where
    absoluteToleranceOf _ = 1e-5
type ApproximateDouble = AbsolutelyApproximateValue MyTolerance Double
-- @-node:gcross.20100717003017.2415:ApproximateDouble
-- @+node:gcross.20100309124842.1404:Bounds
data Bounds = Bounds
    {   boundLeft :: ApproximateDouble
    ,   boundTop :: ApproximateDouble
    ,   boundRight :: ApproximateDouble
    ,   boundBottom :: ApproximateDouble
    } deriving (Show)
-- @-node:gcross.20100309124842.1404:Bounds
-- @+node:gcross.20100302164430.1239:Edge
data Edge = Edge
    {   edgeLeftSide :: EdgeSide
    ,   edgeRightSide :: EdgeSide
    } deriving (Eq,Ord,Show)
-- @-node:gcross.20100302164430.1239:Edge
-- @+node:gcross.20100302164430.1240:EdgeSide
data EdgeSide = EdgeSide
    {   edgeSideVertex :: Vertex
    ,   edgeSideRayNumber :: Int
    } deriving (Eq,Ord,Show)
-- @-node:gcross.20100302164430.1240:EdgeSide
-- @+node:gcross.20100308212437.1389:Lattice
data Lattice = Lattice
    {   latticeVertices :: Set Vertex
    ,   latticeEdges :: [Edge]
    } deriving (Typeable)
-- @-node:gcross.20100308212437.1389:Lattice
-- @+node:gcross.20100308212437.1391:LatticeMonad
type LatticeMonad resultType = State (Lattice,[Step]) resultType
-- @-node:gcross.20100308212437.1391:LatticeMonad
-- @+node:gcross.20100302164430.1241:Step
data Step = Step
    {   stepAngle :: ApproximateDouble -- in degrees
    ,   stepRotation :: ApproximateDouble -- in degrees
    } deriving (Show,Eq)
-- @-node:gcross.20100302164430.1241:Step
-- @+node:gcross.20100302164430.1235:Vertex
data Vertex = Vertex
    {   vertexLocationX :: ApproximateDouble
    ,   vertexLocationY :: ApproximateDouble
    ,   vertexOrientation :: ApproximateDouble
    } deriving (Show,Eq)
-- @-node:gcross.20100302164430.1235:Vertex
-- @+node:gcross.20100714141137.2291:VertexClass(es)
newtype VertexClass = VertexClass { unwrapVertexClass :: [ApproximateDouble] }
newtype VertexClasses = VertexClasses { unwrapVertexClasses :: [VertexClass] }
-- @-node:gcross.20100714141137.2291:VertexClass(es)
-- @+node:gcross.20100714141137.2533:VertexLabelingPermutation
newtype VertexLabelingPermutation = VertexLabelingPermutation
    { unwrapVertexLabelingPermutation :: [Int]
    } deriving (Eq)
-- @-node:gcross.20100714141137.2533:VertexLabelingPermutation
-- @+node:gcross.20100714141137.2534:LatticeLabelingPermutation
newtype LatticeLabelingPermutation = LatticeLabelingPermutation
    { unwrapLatticeLabelingPermutation :: [(Int,VertexLabelingPermutation)]
    } deriving (Eq)
-- @-node:gcross.20100714141137.2534:LatticeLabelingPermutation
-- @-node:gcross.20100302164430.1234:Types
-- @+node:gcross.20100308212437.1383:Instances
-- @+node:gcross.20100308212437.1384:Ord Vertex
instance Ord Vertex where
    compare v1@(Vertex x1 y1 o1) v2@(Vertex x2 y2 o2)
      | location_comparison /= EQ
        = location_comparison
      | o1 == o2
        = EQ
      | otherwise
        = error $
            show v1 ++
            " and "
            ++ show v2 ++
            " are at the same location, but have different orientations!"
      where
        location_comparison = (x1,y1) `compare` (x2,y2)
-- @-node:gcross.20100308212437.1384:Ord Vertex
-- @+node:gcross.20100312175547.1843:Eq Lattice
instance Eq Lattice where
    lattice1 == lattice2 =
        (((==) `on` latticeVertices) lattice1 lattice2)
        &&
        (((==) `on` (sort . latticeEdges)) lattice1 lattice2)
-- @-node:gcross.20100312175547.1843:Eq Lattice
-- @-node:gcross.20100308212437.1383:Instances
-- @+node:gcross.20100302164430.1305:Functions
-- @+node:gcross.20100308212437.1393:Miscellaneous
-- @+node:gcross.20100714141137.1686:(>+<)
(>+<) :: ApproximateDouble → ApproximateDouble → ApproximateDouble
a >+< b = modulo360 (a+b)
-- @-node:gcross.20100714141137.1686:(>+<)
-- @+node:gcross.20100302201317.1255:modulo360
modulo360 :: ApproximateDouble → ApproximateDouble
modulo360 angle = angle - fromIntegral ((floor (angle / 360) :: Int) * 360)
-- @nonl
-- @-node:gcross.20100302201317.1255:modulo360
-- @+node:gcross.20100309124842.1405:withinBounds
withinBounds :: Bounds → Vertex → Bool
withinBounds (Bounds left bottom right top) (Vertex x y _) =
    (x >= left) && (x <= right) &&
    (y >= bottom) && (y <= top)
-- @-node:gcross.20100309124842.1405:withinBounds
-- @+node:gcross.20100309124842.1409:originVertex
originVertex = Vertex 0 0 0
-- @-node:gcross.20100309124842.1409:originVertex
-- @-node:gcross.20100308212437.1393:Miscellaneous
-- @+node:gcross.20100308212437.1394:Stepping
-- @+node:gcross.20100302201317.1252:stepFromVertex
stepFromVertex ::
    Vertex →
    Step →
    Vertex
stepFromVertex (Vertex x y orientation) (Step angle rotation) =
    Vertex
        (x + cos step_angle_in_radians)
        (y + sin step_angle_in_radians)
        (modulo360 $ orientation + rotation)
  where
    step_angle_in_radians = (orientation + angle) / 180 * pi
-- @nonl
-- @-node:gcross.20100302201317.1252:stepFromVertex
-- @+node:gcross.20100302201317.1253:findStepNumberForVertex
findStepNumberForVertex ::
    [Step] →
    Vertex →
    Vertex →
    Int
findStepNumberForVertex steps vertex_to_find vertex_to_step_from = go 0 steps
  where
    go _ [] = error $
        "Unable to find a step in "
        ++ show steps ++
        " from "
        ++ show vertex_to_step_from ++
        " to "
        ++ show vertex_to_find ++
        "."
    go step_number (step:remaining_steps) =
        if (vertexLocation vertex == vertexLocation vertex_to_find)
            then if (vertexOrientation vertex == vertexOrientation vertex_to_find)
                    then step_number
                    else error $
                            ""
                            ++ show step ++
                            " takes us from "
                            ++ show vertex_to_step_from ++
                            " to "
                            ++ show vertex ++
                            ", which is in the same location as "
                            ++ show vertex_to_find ++
                            " but has the wrong orientation!"
            else go (step_number+1) remaining_steps
      where
        vertex = vertex_to_step_from `stepFromVertex` step
-- @-node:gcross.20100302201317.1253:findStepNumberForVertex
-- @-node:gcross.20100308212437.1394:Stepping
-- @+node:gcross.20100308212437.1395:Lattice
-- @+node:gcross.20100308212437.1399:addEdgeToLattice
addEdgeToLattice :: Vertex → Int → Vertex → Int → LatticeMonad ()
addEdgeToLattice vertex1 ray1 vertex2 ray2 =
    modifyLattice (
        \lattice@Lattice{latticeEdges} →
            lattice
            {   latticeEdges =
                    Edge
                        (EdgeSide vertex1 ray1)
                        (EdgeSide vertex2 ray2)
                    :
                    latticeEdges
            }
    )
-- @-node:gcross.20100308212437.1399:addEdgeToLattice
-- @+node:gcross.20100308212437.1401:addVertexToLattice
addVertexToLattice :: Vertex → LatticeMonad ()
addVertexToLattice vertex =
    modifyLattice (
        \lattice@Lattice{latticeVertices} →
            lattice
            {   latticeVertices = Set.insert vertex latticeVertices
            }
    )
-- @-node:gcross.20100308212437.1401:addVertexToLattice
-- @+node:gcross.20100309160622.1347:computeVertexAdjacencies
computeVertexAdjacencies :: Lattice → Map Vertex Int
computeVertexAdjacencies Lattice{..} =
    go latticeEdges
    .
    Map.fromDistinctAscList
    .
    flip zip (repeat 0)
    .
    Set.toList
    $
    latticeVertices
  where
    go [] = id
    go (Edge (EdgeSide v1 _) (EdgeSide v2 _):rest_edges) =
        go rest_edges
        .
        Map.alter increment v1
        .
        Map.alter increment v2

    -- increment Nothing = Just 1
    increment (Just n) = Just (n+1)
-- @-node:gcross.20100309160622.1347:computeVertexAdjacencies
-- @+node:gcross.20100309124842.1330:emptyLattice
emptyLattice :: Lattice
emptyLattice = Lattice Set.empty []
-- @-node:gcross.20100309124842.1330:emptyLattice
-- @+node:gcross.20100714141137.2542:getAllOrientations
getAllOrientations :: LatticeMonad [ApproximateDouble]
getAllOrientations = gets (latticeOrientations . fst)
-- @-node:gcross.20100714141137.2542:getAllOrientations
-- @+node:gcross.20100312175547.1839:getLattice
getLattice :: LatticeMonad Lattice
getLattice = gets fst
-- @-node:gcross.20100312175547.1839:getLattice
-- @+node:gcross.20100312175547.1381:getLatticeSteps
getLatticeSteps :: LatticeMonad [Step]
getLatticeSteps = gets snd
-- @-node:gcross.20100312175547.1381:getLatticeSteps
-- @+node:gcross.20100312133145.1377:getNumberOf[Edges/Vertices]InLattice
getNumberOfEdgesInLattice, getNumberOfVerticesInLattice :: LatticeMonad Int
getNumberOfEdgesInLattice = fmap (length . latticeEdges) getLattice
getNumberOfVerticesInLattice = fmap (Set.size . latticeVertices) getLattice
-- @-node:gcross.20100312133145.1377:getNumberOf[Edges/Vertices]InLattice
-- @+node:gcross.20100309124842.1403:growLatticeToBounds
growLatticeToBounds :: Bounds → [Vertex] → LatticeMonad [Vertex]
growLatticeToBounds bounds = uncurry go . partitionVertices
  where
    partitionVertices = partition (not . withinBounds bounds)

    go outside_vertices [] = return outside_vertices
    go outside_vertices next_vertices =
        fmap partitionVertices (processVertices next_vertices)
        >>=
        \(new_outside_vertices,new_next_vertices) →
            go (new_outside_vertices ++ outside_vertices) new_next_vertices
-- @nonl
-- @-node:gcross.20100309124842.1403:growLatticeToBounds
-- @+node:gcross.20100309124842.1408:growLatticeToBoundsFromOrigin
growLatticeToBoundsFromOrigin :: Bounds → LatticeMonad [Vertex]
growLatticeToBoundsFromOrigin bounds = growLatticeToBounds bounds [originVertex]
-- @-node:gcross.20100309124842.1408:growLatticeToBoundsFromOrigin
-- @+node:gcross.20100331110052.1851:isEmptyLattice
isEmptyLattice :: Lattice → Bool
isEmptyLattice Lattice{..} = Set.null latticeVertices || null latticeEdges
-- @-node:gcross.20100331110052.1851:isEmptyLattice
-- @+node:gcross.20100312133145.1378:iterateLattice
iterateLattice :: [Vertex] → LatticeMonad (Lattice,[Vertex])
iterateLattice starting_vertices = do
    steps ← getLatticeSteps
    starting_number_of_vertices ← getNumberOfVerticesInLattice
    starting_number_of_edges ← getNumberOfEdgesInLattice
    let go bounds vertices = do
            next_vertices ← growLatticeToBounds bounds vertices
            pruned_lattice ← fmap pruneLattice getLattice
            case (numberOfVerticesInLattice pruned_lattice > starting_number_of_vertices
                 ,numberOfEdgesInLattice pruned_lattice > starting_number_of_edges
                 ) of
                (_,True) → return (pruned_lattice,next_vertices)
                (True,False) → error $ "Iteration produced new vertices (post-pruning) without producing more edges, which should never happen."
                (False,False) → go (expandBounds bounds) next_vertices
    go (Bounds (-1) (-1) 1 1) starting_vertices
  where
    expandBounds (Bounds a b c d) = Bounds (a-1) (b-1) (c+1) (d+1)
-- @-node:gcross.20100312133145.1378:iterateLattice
-- @+node:gcross.20100312133145.1380:iterateLatticeRepeatedly
iterateLatticeRepeatedly :: [Vertex] → Int → LatticeMonad ([Lattice],[Vertex])
iterateLatticeRepeatedly vertices =
    go [] vertices
    >=>
    \(lattices,vertices) →
        return (reverse lattices,vertices)
  where
    go lattices current_vertices number_of_iterations_remaining
     | number_of_iterations_remaining <= 0
        = return (lattices,current_vertices)
     | otherwise
        = iterateLattice current_vertices
          >>=
          \(lattice,next_vertices) →
            go (lattice:lattices) next_vertices (number_of_iterations_remaining-1)
-- @-node:gcross.20100312133145.1380:iterateLatticeRepeatedly
-- @+node:gcross.20100308212437.1397:latticeHasVertex
latticeHasVertex :: Vertex → LatticeMonad Bool
latticeHasVertex vertex =
    fmap (
        Set.member vertex
        .
        latticeVertices
    ) getLattice
-- @-node:gcross.20100308212437.1397:latticeHasVertex
-- @+node:gcross.20100714141137.2543:latticeOrientations
latticeOrientations :: Lattice → [ApproximateDouble]
latticeOrientations =
    Set.toList
    .
    Set.map vertexOrientation
    .
    latticeVertices
-- @-node:gcross.20100714141137.2543:latticeOrientations
-- @+node:gcross.20100715150143.1833:latticeRays
latticeRays :: Lattice → [Int]
latticeRays =
    IntSet.toList
    .
    IntSet.fromList
    .
    concat
    .
    map (\(Edge (EdgeSide _ ray1) (EdgeSide _ ray2)) → [ray1,ray2])
    .
    latticeEdges
-- @-node:gcross.20100715150143.1833:latticeRays
-- @+node:gcross.20100312175547.1840:modifyLattice
modifyLattice :: (Lattice → Lattice) → LatticeMonad ()
modifyLattice = modify . first
-- @nonl
-- @-node:gcross.20100312175547.1840:modifyLattice
-- @+node:gcross.20100331110052.1852:numberOfEdgesInLattice
numberOfEdgesInLattice :: Lattice → Int
numberOfEdgesInLattice = length . latticeEdges
-- @nonl
-- @-node:gcross.20100331110052.1852:numberOfEdgesInLattice
-- @+node:gcross.20100714141137.2539:numberOfOrientationsInLattice
numberOfOrientationsInLattice :: Lattice → Int
numberOfOrientationsInLattice =
    length
    .
    latticeOrientations
-- @-node:gcross.20100714141137.2539:numberOfOrientationsInLattice
-- @+node:gcross.20100715150143.1832:numberOfRaysInLattice
numberOfRaysInLattice :: Lattice → Int
numberOfRaysInLattice =
    length
    .
    latticeRays
-- @-node:gcross.20100715150143.1832:numberOfRaysInLattice
-- @+node:gcross.20100331110052.1853:numberOfVerticesInLattice
numberOfVerticesInLattice :: Lattice → Int
numberOfVerticesInLattice = Set.size . latticeVertices
-- @nonl
-- @-node:gcross.20100331110052.1853:numberOfVerticesInLattice
-- @+node:gcross.20100309160622.1351:pruneLattice
pruneLattice :: Lattice → Lattice
pruneLattice lattice@Lattice{..}
    | numberOfEdgesInLattice new_lattice < numberOfEdgesInLattice lattice
        = pruneLattice new_lattice
    | numberOfVerticesInLattice new_lattice < numberOfVerticesInLattice lattice
        = pruneLattice new_lattice
    | otherwise
        = lattice
  where
    vertices_to_remove =
        Map.keysSet
        .
        Map.filter (< 2)
        .
        computeVertexAdjacencies
        $
        lattice

    edgeSideIsStillInLattice :: EdgeSide → Bool
    edgeSideIsStillInLattice =
        flip Set.notMember vertices_to_remove
        .
        edgeSideVertex

    new_lattice =
        Lattice
            (latticeVertices `Set.difference` vertices_to_remove)
            (filter (
                liftA2 ((&&) `on` edgeSideIsStillInLattice)
                    edgeLeftSide
                    edgeRightSide
                ) latticeEdges
            )
-- @nonl
-- @-node:gcross.20100309160622.1351:pruneLattice
-- @+node:gcross.20100309124842.1331:runLatticeMonad
runLatticeMonad :: [Step] → LatticeMonad resultType → (resultType,Lattice)
runLatticeMonad steps = second fst . flip runState (emptyLattice,steps)
-- @-node:gcross.20100309124842.1331:runLatticeMonad
-- @+node:gcross.20100717003017.2445:vertexLocation
vertexLocation :: Vertex → (ApproximateDouble,ApproximateDouble)
vertexLocation = (vertexLocationX &&& vertexLocationY)
-- @-node:gcross.20100717003017.2445:vertexLocation
-- @-node:gcross.20100308212437.1395:Lattice
-- @+node:gcross.20100308212437.1402:Processing Vertices
-- @+node:gcross.20100308212437.1404:processVertex
processVertex :: Vertex → LatticeMonad [Vertex]
processVertex vertex = do
    steps ← getLatticeSteps
    has_vertex ← latticeHasVertex vertex
    if has_vertex
        then return []
        else do
            addVertexToLattice vertex
            let stepped_vertices =
                    map (stepFromVertex vertex) steps
                stepped_vertex_ray_numbers =
                    map (findStepNumberForVertex steps vertex) stepped_vertices
                go queued_vertices
                   ray_number
                   (stepped_vertex:rest_stepped_vertices)
                   (stepped_vertex_ray_number:rest_stepped_vertex_ray_numbers)
                  = latticeHasVertex stepped_vertex
                    >>=
                    \has_stepped_vertex →
                        if has_stepped_vertex
                            then addEdgeToLattice
                                    vertex ray_number
                                    stepped_vertex stepped_vertex_ray_number
                                 >>
                                 recurse queued_vertices
                            else recurse (stepped_vertex:queued_vertices)
                  where
                    recurse new_queued_vertices =
                        go new_queued_vertices
                           (ray_number+1)
                           rest_stepped_vertices
                           rest_stepped_vertex_ray_numbers
                go queued_vertices _ _ _ = return queued_vertices
            go []
               0
               stepped_vertices
               stepped_vertex_ray_numbers
-- @-node:gcross.20100308212437.1404:processVertex
-- @+node:gcross.20100308212437.1468:processVertices
processVertices :: [Vertex] → LatticeMonad [Vertex]
processVertices = fmap concat . mapM processVertex
-- @nonl
-- @-node:gcross.20100308212437.1468:processVertices
-- @-node:gcross.20100308212437.1402:Processing Vertices
-- @+node:gcross.20100713173607.1588:Angle matching
-- @+node:gcross.20100714141137.1604:(?→?)
(?→?) :: VertexClass → VertexClass → Maybe VertexLabelingPermutation
(?→?) (VertexClass angles) =
    fmap VertexLabelingPermutation
    .
    sequence
    .
    map (flip elemIndex angles)
    .
    unwrapVertexClass
-- @-node:gcross.20100714141137.1604:(?→?)
-- @+node:gcross.20100714141137.1607:(??→?)
(??→?) :: VertexClasses → VertexClass → Maybe (Int,VertexLabelingPermutation)
(VertexClasses vcs) ??→? vc2 =
    msum
    .
    zipWith (\index vc1 → fmap (index,) (vc1 ?→? vc2))
        [0..]
    $
    vcs
-- @-node:gcross.20100714141137.1607:(??→?)
-- @+node:gcross.20100714141137.1605:(??→??)
(??→??) :: VertexClasses → VertexClasses → Maybe LatticeLabelingPermutation
(??→??) vertex_classes =
    fmap LatticeLabelingPermutation
    .
    sequence
    .
    map (vertex_classes ??→?)
    .
    unwrapVertexClasses
-- @-node:gcross.20100714141137.1605:(??→??)
-- @+node:gcross.20100713173607.1594:(|⇆)
(|⇆) :: ApproximateDouble → ApproximateDouble → ApproximateDouble
reflection_axis_angle |⇆ angle = 2*reflection_axis_angle - angle
-- @-node:gcross.20100713173607.1594:(|⇆)
-- @+node:gcross.20100714141137.2289:getOriginVertexClassRotatedBy
getOriginVertexClassModifiedBy ::
    (ApproximateDouble → ApproximateDouble) →
    LatticeMonad VertexClass
getOriginVertexClassModifiedBy f =
    fmap (
        VertexClass
        .
        map (
            modulo360
            .
            f
            .
            stepAngle
        )
    ) getLatticeSteps
-- @-node:gcross.20100714141137.2289:getOriginVertexClassRotatedBy
-- @+node:gcross.20100714141137.2290:getAllVertexClassesModifiedBy
getAllVertexClassesModifiedBy ::
    (ApproximateDouble → ApproximateDouble) →
    LatticeMonad VertexClasses
getAllVertexClassesModifiedBy f =
    getAllOrientations
    >>=
    fmap VertexClasses
    .
    mapM (getOriginVertexClassModifiedBy . (f .) . (+))
-- @-node:gcross.20100714141137.2290:getAllVertexClassesModifiedBy
-- @+node:gcross.20100714141137.2532:getAllSymmetricLatticeLabelingPermutations
getAllSymmetricLatticeLabelingPermutations :: Bool → LatticeMonad [LatticeLabelingPermutation]
getAllSymmetricLatticeLabelingPermutations has_reflective_symmetries =
    getAllVertexClassesModifiedBy id
    >>=
    \original_vertex_classes →
        fmap (nub . catMaybes)
        .
        mapM (
            fmap (original_vertex_classes ??→??)
            .
            getAllVertexClassesModifiedBy
        )
        $
        liftM2 (.)
            [id] -- (id : if has_reflective_symmetries then map (|⇆) [0,90] else [])
            -- (map (+) . delete 360 . nub $ [0,30..360] ++ [0,45..360])
            (map (+) . delete 360 . nub $ [0,45..360])
-- @-node:gcross.20100714141137.2532:getAllSymmetricLatticeLabelingPermutations
-- @-node:gcross.20100713173607.1588:Angle matching
-- @-node:gcross.20100302164430.1305:Functions
-- @-others
-- @-node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @-leo

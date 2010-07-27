-- @+leo-ver=4-thin
-- @+node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100312220352.1855:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
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
import qualified Data.Foldable as Fold
import Data.Function
import qualified Data.IntSet as IntSet
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

import Text.Printf

import TypeLevel.NaturalNumber
-- @-node:gcross.20100302164430.1307:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100302164430.1234:Types
-- @+node:gcross.20100717003017.2415:ApproximateDouble
type ApproximateDouble = AbsolutelyApproximateValue (Digits N5) Double
-- @-node:gcross.20100717003017.2415:ApproximateDouble
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
-- @+node:gcross.20100717003017.2453:Periodicity
data Periodicity = Periodicity
    {   periodicityComputeVertexDistance :: (Vertex → ApproximateDouble)
    ,   periodicityWrapVertexAround :: (ApproximateDouble → Vertex → Vertex)
    ,   periodDistance :: ApproximateDouble
    }
-- @-node:gcross.20100717003017.2453:Periodicity
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
    } deriving (Eq)
-- @-node:gcross.20100302164430.1235:Vertex
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
-- @+node:gcross.20100723142502.1638:Show Vertex
instance Show Vertex where
    show (Vertex x y o) =
        "Vertex {("
        ++ show (unwrapAbsolutelyApproximateValue x) ++
        ","
        ++ show (unwrapAbsolutelyApproximateValue y) ++
        ") @ "
        ++ show (unwrapAbsolutelyApproximateValue o) ++
        "}"
-- @-node:gcross.20100723142502.1638:Show Vertex
-- @+node:gcross.20100312175547.1843:Eq Lattice
instance Eq Lattice where
    lattice1 == lattice2 =
        (((==) `on` latticeVertices) lattice1 lattice2)
        &&
        (((==) `on` (sort . map canonicalizeEdge . latticeEdges)) lattice1 lattice2)
-- @-node:gcross.20100312175547.1843:Eq Lattice
-- @+node:gcross.20100723201654.1726:Show Lattice
instance Show Lattice where
    show (Lattice vertices edges) =
        "Lattice:\n\tVertices:\n\t\t"
        ++ (intercalate "\n\t\t"
            .
            map show
            .
            Set.toList
            $
            vertices
            )
        ++
        "\n\tEdges:\n\t\t"
        ++ (intercalate "\n\t\t"
            .
            map show
            $
            edges
           )
        ++
        "\n"
-- @-node:gcross.20100723201654.1726:Show Lattice
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
-- @+node:gcross.20100717003017.2452:canonicalizeEdge
canonicalizeEdge :: Edge → Edge
canonicalizeEdge (Edge s1 s2)
  | s1 > s2   = Edge s2 s1
  | otherwise = Edge s1 s2
-- @-node:gcross.20100717003017.2452:canonicalizeEdge
-- @+node:gcross.20100726103932.1797:canonicalizePeriodicEdge
canonicalizePeriodicEdge :: Periodicity → Int → Edge → Edge
canonicalizePeriodicEdge periodicity radius =
    canonicalizeEdge
    .
    liftA2 Edge
        (canonicalizePeriodicEdgeSide periodicity radius . edgeLeftSide)
        (canonicalizePeriodicEdgeSide periodicity radius . edgeRightSide)
-- @-node:gcross.20100726103932.1797:canonicalizePeriodicEdge
-- @+node:gcross.20100726103932.1798:canonicalizePeriodicEdgeSide
canonicalizePeriodicEdgeSide :: Periodicity → Int → EdgeSide → EdgeSide
canonicalizePeriodicEdgeSide periodicity radius (EdgeSide v r) =
    EdgeSide (canonicalizePeriodicVertex periodicity radius v) r
-- @-node:gcross.20100726103932.1798:canonicalizePeriodicEdgeSide
-- @+node:gcross.20100723201654.1669:canonicalizePeriodicLattice
canonicalizePeriodicLattice :: Periodicity → Int → Lattice → Lattice
canonicalizePeriodicLattice periodicity radius =
    latticeFromEdges
    .
    map (canonicalizePeriodicEdge periodicity radius)
    .
    latticeEdges
-- @-node:gcross.20100723201654.1669:canonicalizePeriodicLattice
-- @+node:gcross.20100726103932.1795:canonicalizePeriodicVertex
canonicalizePeriodicVertex :: Periodicity → Int → Vertex → Vertex 
canonicalizePeriodicVertex Periodicity{..} radius v
  | isBorder v = wrapAround v `min` v
  | otherwise  = v
  where
    wrap_around_distance = fromIntegral radius * periodDistance
    wrapAround = periodicityWrapVertexAround wrap_around_distance
    isBorder = (== wrap_around_distance) . periodicityComputeVertexDistance
-- @-node:gcross.20100726103932.1795:canonicalizePeriodicVertex
-- @+node:gcross.20100726103932.1794:canonicalizePeriodicVertices
canonicalizePeriodicVertices :: Periodicity → Int → Set Vertex → Set Vertex
canonicalizePeriodicVertices periodicity radius =
    Set.map (canonicalizePeriodicVertex periodicity radius)
-- @-node:gcross.20100726103932.1794:canonicalizePeriodicVertices
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
growLatticeToBounds :: (Vertex → Bool) → [Vertex] → LatticeMonad [Vertex]
growLatticeToBounds withinBounds = uncurry go . partitionVertices
  where
    partitionVertices = partition (not . withinBounds)

    go outside_vertices [] = return outside_vertices
    go outside_vertices next_vertices =
        fmap partitionVertices (processVertices next_vertices)
        >>=
        \(new_outside_vertices,new_next_vertices) →
            go (new_outside_vertices ++ outside_vertices) new_next_vertices
-- @nonl
-- @-node:gcross.20100309124842.1403:growLatticeToBounds
-- @+node:gcross.20100309124842.1408:growLatticeToBoundsFromOrigin
growLatticeToBoundsFromOrigin :: (Vertex → Bool) → LatticeMonad [Vertex]
growLatticeToBoundsFromOrigin = flip growLatticeToBounds [originVertex]
-- @-node:gcross.20100309124842.1408:growLatticeToBoundsFromOrigin
-- @+node:gcross.20100723142502.1631:growPeriodicLattice
growPeriodicLattice ::
    Periodicity →
    (ApproximateDouble → ApproximateDouble) →
    ApproximateDouble →
    Int →
    [Vertex] →
    LatticeMonad (Lattice,ApproximateDouble,[Vertex])
growPeriodicLattice
    periodicity@Periodicity{..}
    increaseDistance
    starting_distance
    desired_radius
    = go starting_distance
  where
    go current_distance current_vertices = do
        let withinBounds = (<= current_distance) . periodicityComputeVertexDistance
            next_distance = increaseDistance current_distance
        next_vertices ← growLatticeToBounds withinBounds current_vertices
        lattice ← getLattice
        case periodizeLattice periodicity desired_radius lattice of
            Just periodic_lattice → return (periodic_lattice,next_distance,next_vertices)
            Nothing → go next_distance next_vertices
-- @-node:gcross.20100723142502.1631:growPeriodicLattice
-- @+node:gcross.20100331110052.1851:isEmptyLattice
isEmptyLattice :: Lattice → Bool
isEmptyLattice Lattice{..} = Set.null latticeVertices || null latticeEdges
-- @-node:gcross.20100331110052.1851:isEmptyLattice
-- @+node:gcross.20100723201654.1727:graphLattice
graphLattice :: Lattice → String
graphLattice (Lattice vertices edges) =
    let vertex_labelings =
            Map.fromList
            .
            flip zip ['n':show n | n ← [0..]] -- '
            .
            Set.toList
            $
            vertices
    in "graph {"
        ++ "\n" ++
           intercalate "\n"
            [ printf "%s [pos=\"%f,%f!\"];"
                vertex_label
                (unwrapAbsolutelyApproximateValue vertexLocationX)
                (unwrapAbsolutelyApproximateValue vertexLocationY)
            | (Vertex{..},vertex_label) ← Map.assocs vertex_labelings
            ]
        ++ "\n" ++ 
           intercalate "\n"
            [ printf "%s--%s;"
                (fromJust . flip Map.lookup vertex_labelings $ v1)
                (fromJust . flip Map.lookup vertex_labelings $ v2)
            | (Edge (EdgeSide v1 _) (EdgeSide v2 _)) ← edges
            ]
        ++ "\n}"
-- @-node:gcross.20100723201654.1727:graphLattice
-- @+node:gcross.20100312133145.1378:growLatticeUntilPruningStopsReturningOriginal
growLatticeUntilPruningStopsReturningOriginal ::
    (Vertex → ApproximateDouble) →
    (ApproximateDouble → ApproximateDouble) →
    ApproximateDouble →
    [Vertex] →
    LatticeMonad (Lattice,[Vertex],ApproximateDouble)
growLatticeUntilPruningStopsReturningOriginal
    computeVertexDistance
    increaseDistance
    starting_distance
    starting_vertices
  = do
    steps ← getLatticeSteps
    starting_number_of_vertices ← getNumberOfVerticesInLattice
    starting_number_of_edges ← getNumberOfEdgesInLattice
    let go current_distance vertices = do
            next_vertices ←
                growLatticeToBounds
                    ((< current_distance) . computeVertexDistance)
                    vertices
            pruned_lattice ← fmap pruneLattice getLattice
            case (numberOfVerticesInLattice pruned_lattice > starting_number_of_vertices
                 ,numberOfEdgesInLattice pruned_lattice > starting_number_of_edges
                 ) of
                (_,True) → return (pruned_lattice,next_vertices,current_distance)
                (True,False) → error $ "Iteration produced new vertices (post-pruning) without producing more edges, which should never happen."
                (False,False) → go (increaseDistance current_distance) next_vertices
    go starting_distance starting_vertices
-- @-node:gcross.20100312133145.1378:growLatticeUntilPruningStopsReturningOriginal
-- @+node:gcross.20100312133145.1380:iteratePrunedLattices
iteratePrunedLattices ::
    (Vertex → ApproximateDouble) →
    (ApproximateDouble → ApproximateDouble) →
    ApproximateDouble →
    [Vertex] →
    Int →
    LatticeMonad ([Lattice],[Vertex],ApproximateDouble)
iteratePrunedLattices
    computeVertexDistance
    increaseDistance
    starting_distance
    starting_vertices
  = go [] starting_distance starting_vertices
    >=>
    \(lattices,vertices,distance) →
        return (reverse lattices,vertices,distance)
  where
    go lattices current_distance current_vertices number_of_iterations_remaining
     | number_of_iterations_remaining <= 0
        = return (lattices,current_vertices,current_distance)
     | otherwise
        = growLatticeUntilPruningStopsReturningOriginal
            computeVertexDistance
            increaseDistance
            current_distance
            current_vertices
          >>=
          \(lattice,next_vertices,next_distance) →
            go  (lattice:lattices)
                next_distance
                next_vertices
                (number_of_iterations_remaining-1)
-- @-node:gcross.20100312133145.1380:iteratePrunedLattices
-- @+node:gcross.20100723201654.1670:latticeFromEdges
latticeFromEdges :: [Edge] → Lattice
latticeFromEdges edges = Lattice vertices edges
  where
    vertices =
        Set.unions
        .
        map (\(Edge (EdgeSide v1 _) (EdgeSide v2 _)) →
            Set.fromList [v1,v2]
        )
        $
        edges
-- @-node:gcross.20100723201654.1670:latticeFromEdges
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
-- @+node:gcross.20100717003017.2450:latticeTranslationDistance
latticeTranslationDistance :: Lattice → Maybe ApproximateDouble
latticeTranslationDistance Lattice{latticeVertices}
  | Just (Vertex x1 y1 0,rest_vertices) ←
        Set.minView
        .
        Set.filter ((== 0) . vertexOrientation)
        $
        latticeVertices
  , (not . Set.null) rest_vertices
      = Just
        .
        sqrt
        .
        Set.findMin
        .
        Set.delete 0
        .
        Set.map (\(Vertex x2 y2 0) → (x1-x2)^2 + (y1-y2)^2)
        $
        rest_vertices
  | otherwise
      = Nothing
-- @-node:gcross.20100717003017.2450:latticeTranslationDistance
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
-- @+node:gcross.20100717003017.2449:periodizeLattice
periodizeLattice ::
    Periodicity →
    Int →
    Lattice →
    Maybe Lattice
periodizeLattice
    periodicity@Periodicity
        {   periodicityComputeVertexDistance = computeVertexDistance
        ,   periodicityWrapVertexAround = wrapVertexAround
        ,   periodDistance = translation_distance
        }
    requested_radius
    lattice@Lattice{..}
  | Just (maximum_distance,_) ←
        Set.maxView
        .
        Set.map computeVertexDistance
        $
        latticeVertices
  , maximum_distance >= translation_distance * fromIntegral (requested_radius+2)
     = Just $ let
        new_edges =
            Set.toList
            .
            Set.fromList
            .
            map (canonicalizePeriodicEdge periodicity requested_radius)
            .
            mapMaybe (\edge@(Edge s1@(EdgeSide v1 r1) s2@(EdgeSide v2 r2)) →
                let wrapped_v1 = wrapAround v1
                    wrapped_v2 = wrapAround v2
                in case (placeVertex v1,placeVertex v2) of
                    (GT,GT) → Nothing
                    (EQ,GT) → Nothing
                    (GT,EQ) → Nothing
                    (LT,LT) → Just edge
                    (EQ,EQ) → Just edge
                    (LT,EQ) → Just edge
                    (EQ,LT) → Just edge
                    (LT,GT) → Just (Edge s1 (EdgeSide wrapped_v2 r2))
                    (GT,LT) → Just (Edge (EdgeSide wrapped_v1 r1) s2)
            )
            $
            latticeEdges
          where
            wrap_around_distance = fromIntegral requested_radius * translation_distance
            placeVertex = (`compare` wrap_around_distance) . computeVertexDistance
            wrapAround = wrapVertexAround wrap_around_distance
       in latticeFromEdges new_edges
  | otherwise
     = Nothing
-- @-node:gcross.20100717003017.2449:periodizeLattice
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
-- @-node:gcross.20100302164430.1305:Functions
-- @+node:gcross.20100717003017.2454:Periodicities
-- @+node:gcross.20100722123407.1615:makeComputeDistanceFrom
makeComputeDistanceFrom ::
    [(ApproximateDouble,ApproximateDouble)] →
    Vertex →
    ApproximateDouble
makeComputeDistanceFrom vectors (Vertex x y _) =
    maximum
    .
    map (\(bx,by) → abs (bx*x + by*y))
    $
    vectors
-- @-node:gcross.20100722123407.1615:makeComputeDistanceFrom
-- @+node:gcross.20100722123407.1616:makeReflectiveWrapAroundFrom
makeReflectiveWrapAroundFrom ::
    [(ApproximateDouble,ApproximateDouble)] →
    ApproximateDouble →
    Vertex →
    Vertex
makeReflectiveWrapAroundFrom vectors d vertex = foldl' makeReflectiveWrapAroundFromVector vertex vectors -- '
  where
    makeReflectiveWrapAroundFromVector vertex@(Vertex x y o) (bx,by)
      | abs r < d = vertex
      | otherwise  = Vertex (x + w * bx) (y + w * by) o
      where
        r = (bx*x + by*y)
        w = - 2 * signum r * d
-- @-node:gcross.20100722123407.1616:makeReflectiveWrapAroundFrom
-- @+node:gcross.20100722123407.1631:wrapVertexAroundVector
wrapVertexAroundVector ::
    (ApproximateDouble,ApproximateDouble) →
    ApproximateDouble →
    Vertex →
    Vertex
wrapVertexAroundVector (bx,by) d vertex@(Vertex x y o)
  | abs r < d = vertex
  | otherwise  = Vertex (x - w*bx) (y - w*by) o
 where
    r = bx*x + by*y
    w = 2 * signum r * d
-- @-node:gcross.20100722123407.1631:wrapVertexAroundVector
-- @+node:gcross.20100722123407.1632:rotate
rotate angle_in_degrees (x,y) =
    (x*cos_angle - y*sin_angle, y*cos_angle + x*sin_angle)
  where
    angle = angle_in_degrees / 180 * pi
    cos_angle = cos angle
    sin_angle = sin angle
-- @-node:gcross.20100722123407.1632:rotate
-- @+node:gcross.20100723201654.1666:rectangularPeriodicity
rectangularPeriodicity y_over_x = rectangularPeriodicityRotatedBy y_over_x 0
-- @-node:gcross.20100723201654.1666:rectangularPeriodicity
-- @+node:gcross.20100723201654.1664:rectangularPeriodicityRotatedBy
rectangularPeriodicityRotatedBy y_over_x angle distance =
    let [b1,b2@(b2x,b2y)] = map (rotate angle) [(1,0),(0,1)]

        computeDistanceFrom = makeComputeDistanceFrom [b1,(b2x/y_over_x,b2y/y_over_x)]

        wrapAround d =
            wrapVertexAroundVector b2 (d*y_over_x)
            .
            wrapVertexAroundVector b1 d

    in Periodicity computeDistanceFrom wrapAround distance
-- @-node:gcross.20100723201654.1664:rectangularPeriodicityRotatedBy
-- @+node:gcross.20100723120236.1635:squarePeriodicity
squarePeriodicity = squarePeriodicityRotatedBy 0
-- @-node:gcross.20100723120236.1635:squarePeriodicity
-- @+node:gcross.20100717003017.2455:squarePeriodicityRotatedBy
squarePeriodicityRotatedBy = rectangularPeriodicityRotatedBy 1
-- @-node:gcross.20100717003017.2455:squarePeriodicityRotatedBy
-- @+node:gcross.20100723120236.1637:hexagonalPeriodicity
hexagonalPeriodicity = hexagonalPeriodicityRotatedBy 0
-- @nonl
-- @-node:gcross.20100723120236.1637:hexagonalPeriodicity
-- @+node:gcross.20100722123407.1620:hexagonalPeriodicityRotatedBy
hexagonalPeriodicityRotatedBy angle distance =
    let basis@[b1,b2,b3] = map (rotate angle)
            [(sqrt 3/2,1/2)
            ,(0,1)
            ,(-sqrt 3/2,1/2)
            ]

        computeDistanceFrom = makeComputeDistanceFrom basis

        wrapAround d vertex@(Vertex x y _)
          | offsetIs   0 = wrapVertexAroundVector b1 d vertex
          | offsetIs  60 = wrapVertexAroundVector b2 d vertex
          | offsetIs 120 = wrapVertexAroundVector b3 d vertex
          | otherwise    =
                error (
                    "Cannot wrap vertex around distance "
                    ++ show (unwrapAbsolutelyApproximateValue d) ++
                    " because it is located at "
                    ++ show (((,) `on` unwrapAbsolutelyApproximateValue) x y) ++
                    " @ "
                    ++ show (unwrapAbsolutelyApproximateValue $ atan2 y x / pi * 180) ++
                    ", which is on a (hexagonal) corner"
                )
          where
            vertex_angle = modulo360 (atan2 y x/pi*180 - angle)
            isInside a b = (a < vertex_angle) && (vertex_angle < b)
            offsetIs offset =
                isInside offset (offset+60)
             || isInside (offset+180) (offset+240)

    in Periodicity computeDistanceFrom wrapAround distance
-- @-node:gcross.20100722123407.1620:hexagonalPeriodicityRotatedBy
-- @-node:gcross.20100717003017.2454:Periodicities
-- @-others
-- @-node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @-leo

-- @+leo-ver=4-thin
-- @+node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100312220352.1855:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100312220352.1855:<< Language extensions >>
-- @nl

module CodeLattice where

-- @<< Import needed modules >>
-- @+node:gcross.20100302164430.1307:<< Import needed modules >>
import Control.Applicative.Infix
import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Char
import Data.Either
import Data.EpsilonMatcher
import Data.EpsilonMatcher.Multiple
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

import Debug.Trace
-- @-node:gcross.20100302164430.1307:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100302164430.1234:Types
-- @+node:gcross.20100302164430.1237:Angle
type Angle = Int
-- @-node:gcross.20100302164430.1237:Angle
-- @+node:gcross.20100309124842.1404:Bounds
data Bounds = Bounds
    {   boundLeft :: Double
    ,   boundTop :: Double
    ,   boundRight :: Double
    ,   boundBottom :: Double
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
    {   edgeSideVertexNumber :: Int
    ,   edgeSideRayNumber :: Int
    } deriving (Eq,Ord,Show)
-- @-node:gcross.20100302164430.1240:EdgeSide
-- @+node:gcross.20100308212437.1389:Lattice
data Lattice = Lattice
    {   latticeVertices :: Bimap Int Vertex
    ,   latticeEdges :: [Edge]
    } deriving (Typeable)

-- @-node:gcross.20100308212437.1389:Lattice
-- @+node:gcross.20100308212437.1391:LatticeMonad
type LatticeMonad resultType = StateT (Lattice,[Step]) (State (IntMap (EpsilonMatcher Double))) resultType
-- @-node:gcross.20100308212437.1391:LatticeMonad
-- @+node:gcross.20100302164430.1236:Location
data Location = Location
    {   locationX :: Int
    ,   locationY :: Int
    } deriving (Eq,Ord,Show)

-- @-node:gcross.20100302164430.1236:Location
-- @+node:gcross.20100327142350.1553:PositionSpaceLattice
newtype PositionSpaceLattice = PositionSpaceLattice { unwrapPositionSpaceLattice :: Lattice } deriving (Eq,Typeable)
-- @-node:gcross.20100327142350.1553:PositionSpaceLattice
-- @+node:gcross.20100302164430.1242:RawVertex
data RawVertex = RawVertex
    {   rawVertexX :: Double
    ,   rawVertexY :: Double
    ,   rawVertexOrientation :: Double
    } deriving (Eq,Ord,Show)

-- @-node:gcross.20100302164430.1242:RawVertex
-- @+node:gcross.20100302201317.1254:ResolverMonad
type ResolverMonad resultType = MultipleEpsilonMatcherState Double resultType
-- @-node:gcross.20100302201317.1254:ResolverMonad
-- @+node:gcross.20100302164430.1241:Step
data Step = Step
    {   stepAngle :: Double -- in degrees
    ,   stepRotation :: Double -- in degrees
    } deriving (Show,Eq)

-- @-node:gcross.20100302164430.1241:Step
-- @+node:gcross.20100302164430.1235:Vertex
data Vertex = Vertex
    {   vertexLocation :: Location
    ,   vertexOrientation :: Angle
    } deriving (Show,Eq)
-- @-node:gcross.20100302164430.1235:Vertex
-- @-node:gcross.20100302164430.1234:Types
-- @+node:gcross.20100308212437.1383:Instances
-- @+node:gcross.20100308212437.1384:Ord Vertex
instance Ord Vertex where
    compare v1@(Vertex l1 o1) v2@(Vertex l2 o2)
     | l1 `compare` l2 /= EQ
        = l1 `compare` l2
     | o1 == o2
        = EQ
     | otherwise
        = error $
            show v1 ++
            " and "
            ++ show v2 ++
            " are at the same location, but have different orientations!"
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
-- @+node:gcross.20100302201317.1255:modulo360
modulo360 :: Double → Double
modulo360 angle = angle - fromIntegral ((floor (angle / 360) :: Int) * 360)
-- @nonl
-- @-node:gcross.20100302201317.1255:modulo360
-- @+node:gcross.20100309124842.1405:withinBounds
withinBounds :: Bounds → RawVertex → Bool
withinBounds (Bounds left bottom right top) (RawVertex x y _) =
    (x >= (left-epsilon)) && (x <= (right+epsilon)) &&
    (y >= (bottom-epsilon)) && (y <= (top+epsilon))
  where
    epsilon = 1e-10
-- @nonl
-- @-node:gcross.20100309124842.1405:withinBounds
-- @+node:gcross.20100309124842.1409:originRawVertex
originRawVertex = RawVertex 0 0 0
-- @-node:gcross.20100309124842.1409:originRawVertex
-- @-node:gcross.20100308212437.1393:Miscellaneous
-- @+node:gcross.20100308212437.1392:Resolving
-- @+node:gcross.20100302164430.1306:resolveVertex
resolveVertex :: RawVertex → ResolverMonad Vertex
resolveVertex (RawVertex x y angle) =
    liftM2 Vertex
        (liftM2 Location
            (lookupMatchIn 0 x)
            (lookupMatchIn 1 y)
        )
        (lookupMatchIn 2 . modulo360 $ angle)
-- @nonl
-- @-node:gcross.20100302164430.1306:resolveVertex
-- @+node:gcross.20100306220637.1354:runResolverMonad
runResolverMonad :: ResolverMonad resultType → (resultType,[IntMap Int])
runResolverMonad = runMultipleEpsilonMatchers [1e-5,1e-5,1e-5]
-- @nonl
-- @-node:gcross.20100306220637.1354:runResolverMonad
-- @-node:gcross.20100308212437.1392:Resolving
-- @+node:gcross.20100308212437.1394:Stepping
-- @+node:gcross.20100302201317.1252:stepFromRawVertex
stepFromRawVertex ::
    RawVertex →
    Step →
    RawVertex
stepFromRawVertex (RawVertex x y orientation) (Step angle rotation) =
    RawVertex
        (x + cos step_angle_in_radians)
        (y + sin step_angle_in_radians)
        (modulo360 $ orientation + rotation)
  where
    step_angle_in_radians = (orientation + angle) / 180 * pi
-- @nonl
-- @-node:gcross.20100302201317.1252:stepFromRawVertex
-- @+node:gcross.20100302201317.1253:findStepNumberForRawVertex
findStepNumberForRawVertex ::
    [Step] →
    RawVertex →
    RawVertex →
    ResolverMonad Int
findStepNumberForRawVertex steps vertex_to_find vertex_to_step_from = do
    resolved_vertex_to_find ← resolveVertex vertex_to_find
    let go _ [] = error $
            "Unable to find a step in "
            ++ show steps ++
            " from "
            ++ show vertex_to_step_from ++
            " to "
            ++ show vertex_to_find ++
            "."
        go step_number (step:remaining_steps) =
            resolveVertex vertex
            >>=
            \resolved_vertex →
                if (vertexLocation resolved_vertex == vertexLocation resolved_vertex_to_find)
                    then if (vertexOrientation resolved_vertex == vertexOrientation resolved_vertex_to_find)
                            then return step_number
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
            vertex = vertex_to_step_from `stepFromRawVertex` step
    go 0 steps
-- @nonl
-- @-node:gcross.20100302201317.1253:findStepNumberForRawVertex
-- @-node:gcross.20100308212437.1394:Stepping
-- @+node:gcross.20100308212437.1395:Lattice
-- @+node:gcross.20100309124842.1330:emptyLattice
emptyLattice :: Lattice
emptyLattice = Lattice Bimap.empty []
-- @-node:gcross.20100309124842.1330:emptyLattice
-- @+node:gcross.20100331110052.1851:isEmptyLattice
isEmptyLattice :: Lattice → Bool
isEmptyLattice = (Bimap.null . latticeVertices) <^(||)^> (null . latticeEdges)
-- @nonl
-- @-node:gcross.20100331110052.1851:isEmptyLattice
-- @+node:gcross.20100331110052.1852:latticeNumberOfEdges
latticeNumberOfEdges :: Lattice → Int
latticeNumberOfEdges = length . latticeEdges
-- @nonl
-- @-node:gcross.20100331110052.1852:latticeNumberOfEdges
-- @+node:gcross.20100331110052.1853:latticeNumberOfVertices
latticeNumberOfVertices :: Lattice → Int
latticeNumberOfVertices = Bimap.size . latticeVertices
-- @nonl
-- @-node:gcross.20100331110052.1853:latticeNumberOfVertices
-- @+node:gcross.20100308212437.1397:latticeHasVertex
latticeHasVertex :: Vertex → LatticeMonad Bool
latticeHasVertex vertex = fmap (Bimap.memberR vertex . latticeVertices) (gets fst)
-- @-node:gcross.20100308212437.1397:latticeHasVertex
-- @+node:gcross.20100312175547.1840:modifyLattice
modifyLattice :: (Lattice → Lattice) → LatticeMonad ()
modifyLattice = modify . first
-- @nonl
-- @-node:gcross.20100312175547.1840:modifyLattice
-- @+node:gcross.20100308212437.1399:addEdgeToLattice
addEdgeToLattice :: Vertex → Int → Vertex → Int → LatticeMonad ()
addEdgeToLattice vertex1 ray1 vertex2 ray2 =
    liftM2 Edge
        (liftM (flip EdgeSide ray1) . getVertexNumberInLattice $ vertex1)
        (liftM (flip EdgeSide ray2) . getVertexNumberInLattice $ vertex2)
    >>=
    \edge →
        modifyLattice (
            \lattice →
                lattice
                {   latticeEdges =
                        edge:(latticeEdges lattice)
                }
        )
-- @nonl
-- @-node:gcross.20100308212437.1399:addEdgeToLattice
-- @+node:gcross.20100312175547.1838:getVertexNumberInLattice
getVertexNumberInLattice :: Vertex → LatticeMonad Int
getVertexNumberInLattice vertex = fmap (fromJust . Bimap.lookupR vertex . latticeVertices) getLattice
-- @nonl
-- @-node:gcross.20100312175547.1838:getVertexNumberInLattice
-- @+node:gcross.20100308212437.1401:addVertexToLattice
addVertexToLattice :: Vertex → LatticeMonad ()
addVertexToLattice vertex =
    modifyLattice (
        \lattice@(Lattice vertices _) →
            lattice
            {   latticeVertices =
                    Bimap.insert (Bimap.size vertices) vertex vertices
            }
    )
-- @nonl
-- @-node:gcross.20100308212437.1401:addVertexToLattice
-- @+node:gcross.20100309124842.1331:runLatticeMonad
runLatticeMonad :: [Step] → LatticeMonad resultType → ((resultType,Lattice),[IntMap Int])
runLatticeMonad steps = (first . second $ fst) . runResolverMonad . flip runStateT (emptyLattice,steps)
-- @nonl
-- @-node:gcross.20100309124842.1331:runLatticeMonad
-- @+node:gcross.20100309124842.1403:growLatticeToBounds
growLatticeToBounds :: Bounds → [RawVertex] → LatticeMonad [RawVertex]
growLatticeToBounds bounds = uncurry go . partitionRawVertices
  where
    partitionRawVertices = partitionEithers . map placeRawVertex

    placeRawVertex raw_vertex =
        if withinBounds bounds raw_vertex
            then Right raw_vertex
            else Left raw_vertex

    go outside_raw_vertices [] = return outside_raw_vertices
    go outside_raw_vertices next_raw_vertices =
        fmap partitionRawVertices (processRawVertices next_raw_vertices)
        >>=
        \(new_outside_vertices,new_next_vertices) →
            go (new_outside_vertices ++ outside_raw_vertices) new_next_vertices
-- @nonl
-- @-node:gcross.20100309124842.1403:growLatticeToBounds
-- @+node:gcross.20100309124842.1408:growLatticeToBoundsFromOrigin
growLatticeToBoundsFromOrigin :: Bounds → LatticeMonad [RawVertex]
growLatticeToBoundsFromOrigin bounds = growLatticeToBounds bounds [RawVertex 0 0 0]
-- @nonl
-- @-node:gcross.20100309124842.1408:growLatticeToBoundsFromOrigin
-- @+node:gcross.20100309160622.1347:computeVertexAdjacencies
computeVertexAdjacencies :: Lattice → IntMap Int
computeVertexAdjacencies (Lattice vertices edges) =
    go edges (IntMap.fromDistinctAscList . map (id *** const 0) . Map.toAscList . Bimap.toMap $ vertices)
  where
    go [] = id
    go (Edge (EdgeSide v1 _) (EdgeSide v2 _):rest_edges) =
        go rest_edges
        .
        IntMap.alter increment v1
        .
        IntMap.alter increment v2

    increment Nothing = Just 1
    increment (Just n) = Just (n+1)
-- @nonl
-- @-node:gcross.20100309160622.1347:computeVertexAdjacencies
-- @+node:gcross.20100309160622.1351:pruneLattice
pruneLattice :: Lattice → Lattice
pruneLattice lattice@(Lattice vertices edges)
    | (length . latticeEdges $ new_lattice) < length edges
        = pruneLattice new_lattice
    | (Bimap.size . latticeVertices $ new_lattice) < Bimap.size vertices
        = pruneLattice new_lattice
    | otherwise
        = lattice
  where
    vertices_to_remove =
        IntMap.filter (< 2)
        .
        computeVertexAdjacencies
        $
        lattice

    new_lattice =
        lattice
            {   latticeVertices = 
                    foldl' (flip Bimap.delete) vertices . IntMap.keys $ vertices_to_remove -- '
            ,   latticeEdges =
                    filter (
                        \(Edge (EdgeSide v1 _) (EdgeSide v2 _)) →
                            (IntMap.notMember v1 vertices_to_remove)
                            &&
                            (IntMap.notMember v2 vertices_to_remove)
                    ) edges
            }
-- @nonl
-- @-node:gcross.20100309160622.1351:pruneLattice
-- @+node:gcross.20100310123433.1421:drawLattice
drawLattice :: PositionSpaceLattice → String
drawLattice (PositionSpaceLattice lattice)
  | (Bimap.null . latticeVertices) lattice = ""
  | otherwise =
    let coordinate_map = 
            Map.fromList
            .
            map (((locationX &&& locationY) . vertexLocation) &&& vertexOrientation)
            .
            Bimap.elems
            .
            latticeVertices
            $
            lattice
        (min_X,max_X) = (minimum &&& maximum) . map fst . Map.keys $ coordinate_map
        (min_Y,max_Y) = (minimum &&& maximum) . map snd . Map.keys $ coordinate_map
    in  unlines
        .
        transpose
        .
        removeBlankLines
        .
        transpose
        .
        removeBlankLines
        $
        [[maybe ' ' (chr . (+ ord '0')) (Map.lookup (x,y) coordinate_map)
         | x ← [min_X..max_X]
         ]
        | y ← [max_Y,max_Y-1..min_Y]
        ]
  where
    removeBlankLines = filter (any (/= ' '))
-- @nonl
-- @-node:gcross.20100310123433.1421:drawLattice
-- @+node:gcross.20100312175547.1839:getLattice
getLattice :: LatticeMonad Lattice
getLattice = gets fst
-- @-node:gcross.20100312175547.1839:getLattice
-- @+node:gcross.20100331165456.1577:getPositionSpaceLattice
getPositionSpaceLattice :: LatticeMonad PositionSpaceLattice
getPositionSpaceLattice =
    lift getMatchMaps
    >>=
    \[x_map,y_map,orientation_map] →
        fmap (mapKeysToPositionsInLattice x_map y_map orientation_map) getLattice
-- @nonl
-- @-node:gcross.20100331165456.1577:getPositionSpaceLattice
-- @+node:gcross.20100312175547.1381:getLatticeSteps
getLatticeSteps :: LatticeMonad [Step]
getLatticeSteps = gets snd
-- @-node:gcross.20100312175547.1381:getLatticeSteps
-- @+node:gcross.20100310140947.1418:getAndDrawLattice
getAndDrawLattice :: LatticeMonad String
getAndDrawLattice = fmap drawLattice getPositionSpaceLattice
-- @-node:gcross.20100310140947.1418:getAndDrawLattice
-- @+node:gcross.20100310140947.1420:getAndDrawPrunedLattice
getAndDrawPrunedLattice :: LatticeMonad String
getAndDrawPrunedLattice = 
    lift getMatchMaps
    >>=
    \[x_map,y_map,orientation_map] →
        fmap (drawLattice . mapKeysToPositionsInLattice x_map y_map orientation_map . pruneLattice) getLattice
-- @nonl
-- @-node:gcross.20100310140947.1420:getAndDrawPrunedLattice
-- @+node:gcross.20100312133145.1377:getNumberOf[Edges/Vertices]InLattice
getNumberOfEdgesInLattice, getNumberOfVerticesInLattice :: LatticeMonad Int
getNumberOfEdgesInLattice = fmap (length . latticeEdges) getLattice
getNumberOfVerticesInLattice = fmap (Bimap.size . latticeVertices) getLattice
-- @-node:gcross.20100312133145.1377:getNumberOf[Edges/Vertices]InLattice
-- @+node:gcross.20100312133145.1378:iterateLattice
iterateLattice :: [RawVertex] → LatticeMonad (Lattice,[RawVertex])
iterateLattice starting_raw_vertices = do
    steps ← getLatticeSteps
    starting_number_of_vertices ← getNumberOfVerticesInLattice
    starting_number_of_edges ← getNumberOfEdgesInLattice
    let go bounds raw_vertices = do
            next_raw_vertices ← growLatticeToBounds bounds raw_vertices
            pruned_lattice ← fmap pruneLattice getLattice
            case ((Bimap.size . latticeVertices) pruned_lattice > starting_number_of_vertices
                 ,(length . latticeEdges) pruned_lattice > starting_number_of_edges
                 ) of
                (_,True) → return (pruned_lattice,next_raw_vertices)
                (True,False) → error $ "Iteration produced new vertices (post-pruning) without producing more edges, which should never happen."
                (False,False) → go (expandBounds bounds) next_raw_vertices
    go (Bounds (-1) (-1) 1 1) starting_raw_vertices
  where
    expandBounds (Bounds a b c d) = Bounds (a-1) (b-1) (c+1) (d+1)
-- @nonl
-- @-node:gcross.20100312133145.1378:iterateLattice
-- @+node:gcross.20100312133145.1380:iterateLatticeRepeatedly
iterateLatticeRepeatedly :: [RawVertex] → Int → LatticeMonad ([Lattice],[RawVertex])
iterateLatticeRepeatedly raw_vertices =
    go [] raw_vertices
    >=>
    \(lattices,raw_vertices) →
        return (reverse lattices,raw_vertices)
  where
    go lattices current_raw_vertices number_of_iterations_remaining
     | number_of_iterations_remaining <= 0
        = return (lattices,current_raw_vertices)
     | otherwise
        = iterateLattice current_raw_vertices
          >>=
          \(lattice,next_raw_vertices) →
            go (lattice:lattices) next_raw_vertices (number_of_iterations_remaining-1)
-- @nonl
-- @-node:gcross.20100312133145.1380:iterateLatticeRepeatedly
-- @+node:gcross.20100312175547.1828:mapKeysToPositionInLattice
mapKeysToPositionsInLattice :: MatchMap → MatchMap → MatchMap → Lattice → PositionSpaceLattice
mapKeysToPositionsInLattice x_map y_map orientation_map lattice =
    PositionSpaceLattice
    $
    lattice
    {   latticeVertices =
            Bimap.fromList
            .
            map (second mapKeysToPositionsInVertex)
            .
            Bimap.toList
            .
            latticeVertices
            $
            lattice
    }
  where
    mapKeysToPositionsInVertex (Vertex (Location x_key y_key) orientation_key) =
        Vertex (Location x y) orientation
      where
        x = fromJust (IntMap.lookup x_key x_map)
        y = fromJust (IntMap.lookup y_key y_map)
        orientation = fromJust (IntMap.lookup orientation_key orientation_map)
-- @nonl
-- @-node:gcross.20100312175547.1828:mapKeysToPositionInLattice
-- @+node:gcross.20100330162705.1550:periodizeLatticeGrownWithinRectangularBounds
periodizeLatticeGrownWithinRectangularBounds :: PositionSpaceLattice → PositionSpaceLattice
periodizeLatticeGrownWithinRectangularBounds (PositionSpaceLattice (Lattice vertices edges))
  | period_width == 0 || period_height == 0
    = PositionSpaceLattice $ emptyLattice
  | otherwise
    = PositionSpaceLattice $ Lattice filtered_vertices filtered_edges
  where
    locations_with_orientation_zero =
        map vertexLocation
        .
        filter ((== 0) . vertexOrientation)
        .
        Bimap.elems
        $
        vertices
    Location minX minY = minimum locations_with_orientation_zero

    findMaximumCoordinate minA locationA locationB =
        head
        .
        (\lst →
            case lst of
                (x:rest@(_:_)) → rest
                _ → lst
        )
        .
        reverse
        .
        sort
        .
        mapMaybe (
            \location →
                if locationA location == minA
                    then Just (locationB location)
                    else Nothing
        )
        $
        locations_with_orientation_zero
    maxX = findMaximumCoordinate minY locationY locationX
    maxY = findMaximumCoordinate minX locationX locationY

    period_width = maxX - minX
    period_height = maxY - minY

    filtered_vertices =
        Bimap.fromList
        .
        mapMaybe
            (\(vertex_number,Vertex (Location x y) orientation) →
                if (x >= minX) && (x < maxX) &&
                   (y >= minY) && (y < maxY)
                    then Just (vertex_number,Vertex (Location (x-minX) (y-minY)) orientation)
                    else Nothing
            )
        .
        Bimap.assocs
        $
        vertices

    filtered_edges =
        mapMaybe
            (\edge@(Edge s1@(EdgeSide v1 r1) s2@(EdgeSide v2 r2)) →
                case (Bimap.member v1 filtered_vertices,Bimap.member v2 filtered_vertices) of
                    (True,True) → Just edge
                    (False,False) → Nothing
                    (True,False) → periodizeEdge s1 r2 . fromJust $ Bimap.lookup v2 vertices
                    (False,True) → periodizeEdge s2 r1 . fromJust $ Bimap.lookup v1 vertices
            )
        $
        edges
      where
        periodizeEdge interior_edge_side exterior_ray_number (Vertex (Location x y) orientation)
          | x < minX || y < minY
            = Nothing
          | otherwise
            = let wrapped_x = if x < maxX then x else x-period_width
                  wrapped_y = if y < maxY then y else y-period_height
                  wrapped_vertex = Vertex (Location wrapped_x wrapped_y) orientation
                  wrapped_vertex_number = fromJust $ Bimap.lookupR wrapped_vertex vertices
              in Just $ Edge interior_edge_side (EdgeSide wrapped_vertex_number exterior_ray_number)
-- @nonl
-- @-node:gcross.20100330162705.1550:periodizeLatticeGrownWithinRectangularBounds
-- @+node:gcross.20100331165456.1579:iteratePeriodicLattice
iteratePeriodicLattice :: [RawVertex] → LatticeMonad (PositionSpaceLattice,[RawVertex])
iteratePeriodicLattice starting_raw_vertices = do
    steps ← getLatticeSteps
    starting_number_of_vertices ← getNumberOfVerticesInLattice
    starting_number_of_edges ← getNumberOfEdgesInLattice
    let go bounds raw_vertices = do
            next_raw_vertices ← growLatticeToBounds bounds raw_vertices
            pruned_lattice ← fmap periodizeLatticeGrownWithinRectangularBounds getPositionSpaceLattice
            case ((Bimap.size . latticeVertices . unwrapPositionSpaceLattice) pruned_lattice > starting_number_of_vertices
                 ,(length . latticeEdges . unwrapPositionSpaceLattice) pruned_lattice > starting_number_of_edges
                 ) of
                (_,True) → return (pruned_lattice,next_raw_vertices)
                (True,False) → error $ "Iteration produced new vertices (post-pruning) without producing more edges, which should never happen."
                (False,False) → go (expandBounds bounds) next_raw_vertices
    go (Bounds (-1) (-1) 1 1) starting_raw_vertices
  where
    expandBounds (Bounds a b c d) = Bounds (a-1) (b-1) (c+1) (d+1)
-- @nonl
-- @-node:gcross.20100331165456.1579:iteratePeriodicLattice
-- @+node:gcross.20100331165456.1581:iterateLatticeRepeatedly
iteratePeriodicLatticeRepeatedly :: [RawVertex] → Int → LatticeMonad ([PositionSpaceLattice],[RawVertex])
iteratePeriodicLatticeRepeatedly raw_vertices =
    go [] raw_vertices
    >=>
    \(lattices,raw_vertices) →
        return (reverse lattices,raw_vertices)
  where
    go lattices current_raw_vertices number_of_iterations_remaining
     | number_of_iterations_remaining <= 0
        = return (lattices,current_raw_vertices)
     | otherwise
        = iteratePeriodicLattice current_raw_vertices
          >>=
          \(lattice,next_raw_vertices) →
            go (lattice:lattices) next_raw_vertices (number_of_iterations_remaining-1)
-- @nonl
-- @-node:gcross.20100331165456.1581:iterateLatticeRepeatedly
-- @-node:gcross.20100308212437.1395:Lattice
-- @+node:gcross.20100308212437.1402:Processing Vertices
-- @+node:gcross.20100308212437.1404:processRawVertex
processRawVertex :: RawVertex → LatticeMonad [RawVertex]
processRawVertex raw_vertex = do
    steps ← getLatticeSteps
    vertex ← lift (resolveVertex raw_vertex)
    has_vertex ← latticeHasVertex vertex
    if has_vertex
        then return []
        else do
            addVertexToLattice vertex
            let stepped_raw_vertices = map (stepFromRawVertex raw_vertex) steps
            stepped_vertices ← lift (mapM resolveVertex stepped_raw_vertices)
            stepped_vertex_ray_numbers ← lift (mapM (findStepNumberForRawVertex steps raw_vertex) stepped_raw_vertices)
            let go queued_raw_vertices
                   ray_number
                   (stepped_vertex:rest_stepped_vertices)
                   (stepped_raw_vertex:rest_stepped_raw_vertices)
                   (stepped_vertex_ray_number:rest_stepped_vertex_ray_numbers)
                  = latticeHasVertex stepped_vertex
                    >>=
                    \has_stepped_vertex →
                        if has_stepped_vertex
                            then addEdgeToLattice
                                    vertex ray_number
                                    stepped_vertex stepped_vertex_ray_number
                                 >>
                                 recurse queued_raw_vertices
                            else recurse (stepped_raw_vertex:queued_raw_vertices)
                  where
                    recurse new_queued_raw_vertices =
                        go new_queued_raw_vertices
                           (ray_number+1)
                           rest_stepped_vertices
                           rest_stepped_raw_vertices
                           rest_stepped_vertex_ray_numbers
                go queued_raw_vertices _ _ _ _ = return queued_raw_vertices
            go []
               0
               stepped_vertices
               stepped_raw_vertices
               stepped_vertex_ray_numbers
-- @nonl
-- @-node:gcross.20100308212437.1404:processRawVertex
-- @+node:gcross.20100308212437.1468:processRawVertices
processRawVertices :: [RawVertex] → LatticeMonad [RawVertex]
processRawVertices = fmap concat . mapM processRawVertex
-- @nonl
-- @-node:gcross.20100308212437.1468:processRawVertices
-- @-node:gcross.20100308212437.1402:Processing Vertices
-- @-node:gcross.20100302164430.1305:Functions
-- @-others
-- @-node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @-leo

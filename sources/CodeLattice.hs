-- @+leo-ver=4-thin
-- @+node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @@language Haskell

module CodeLattice where

-- @<< Import needed modules >>
-- @+node:gcross.20100302164430.1307:<< Import needed modules >>
import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict

import Data.EpsilonMatcher
import Data.EpsilonMatcher.Multiple
import Data.IntMap (IntMap)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
-- @-node:gcross.20100302164430.1307:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100302164430.1234:Types
-- @+node:gcross.20100302164430.1237:Angle
type Angle = Int
-- @-node:gcross.20100302164430.1237:Angle
-- @+node:gcross.20100302164430.1239:Edge
data Edge = Edge
    {   edgeLeftSide :: EdgeSide
    ,   edgeRightSide :: EdgeSide
    }
-- @-node:gcross.20100302164430.1239:Edge
-- @+node:gcross.20100302164430.1240:EdgeSide
data EdgeSide = EdgeSide
    {   edgeSideVertex :: Vertex
    ,   edgeSideRayNumber :: Int
    }
-- @-node:gcross.20100302164430.1240:EdgeSide
-- @+node:gcross.20100308212437.1389:Lattice
data Lattice = Lattice
    {   latticeVertices :: Set Vertex
    ,   latticeEdges :: [Edge]
    }
-- @-node:gcross.20100308212437.1389:Lattice
-- @+node:gcross.20100308212437.1391:LatticeMonad
type LatticeMonad resultType = StateT Lattice (State (IntMap (EpsilonMatcher Double))) resultType
-- @-node:gcross.20100308212437.1391:LatticeMonad
-- @+node:gcross.20100302164430.1236:Location
data Location = Location
    {   locationX :: Int
    ,   locationY :: Int
    } deriving (Eq,Ord,Show)

-- @-node:gcross.20100302164430.1236:Location
-- @+node:gcross.20100302164430.1242:RawVertex
data RawVertex = RawVertex
    {   rawVertexX :: Double
    ,   rawVertexY :: Double
    ,   rawVertexOrientation :: Double
    } deriving (Show)

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
-- @-node:gcross.20100308212437.1383:Instances
-- @+node:gcross.20100302164430.1305:Functions
-- @+node:gcross.20100308212437.1393:Miscellaneous
-- @+node:gcross.20100302201317.1255:modulo360
modulo360 :: Double -> Double
modulo360 angle = angle - fromIntegral ((floor (angle / 360) :: Int) * 360)
-- @-node:gcross.20100302201317.1255:modulo360
-- @-node:gcross.20100308212437.1393:Miscellaneous
-- @+node:gcross.20100308212437.1392:Resolving
-- @+node:gcross.20100302164430.1306:resolveVertex
resolveVertex :: RawVertex -> ResolverMonad Vertex
resolveVertex (RawVertex x y angle) =
    liftM2 Vertex
        (liftM2 Location
            (lookupMatchIn 0 x)
            (lookupMatchIn 1 y)
        )
        (lookupMatchIn 2 . modulo360 $ angle)
-- @-node:gcross.20100302164430.1306:resolveVertex
-- @+node:gcross.20100306220637.1354:runResolverMonad
runResolverMonad :: ResolverMonad resultType -> (resultType,[IntMap Int])
runResolverMonad = runMultipleEpsilonMatchers [1e-5,1e-5,1e-5]
-- @-node:gcross.20100306220637.1354:runResolverMonad
-- @-node:gcross.20100308212437.1392:Resolving
-- @+node:gcross.20100308212437.1394:Stepping
-- @+node:gcross.20100302201317.1252:stepFromRawVertex
stepFromRawVertex ::
    RawVertex ->
    Step ->
    RawVertex
stepFromRawVertex (RawVertex x y orientation) (Step angle rotation) =
    RawVertex
        (x + cos step_angle_in_radians)
        (y + sin step_angle_in_radians)
        (modulo360 $ orientation + rotation)
  where
    step_angle_in_radians = (orientation + angle) / 180 * pi
-- @-node:gcross.20100302201317.1252:stepFromRawVertex
-- @+node:gcross.20100302201317.1253:findStepNumberForRawVertex
findStepNumberForRawVertex ::
    [Step] ->
    RawVertex ->
    RawVertex ->
    ResolverMonad Int
findStepNumberForRawVertex steps vertex_to_find vertex_to_step_from = do
    resolved_vertex_to_find <- resolveVertex vertex_to_find
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
            \resolved_vertex ->
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
-- @-node:gcross.20100302201317.1253:findStepNumberForRawVertex
-- @-node:gcross.20100308212437.1394:Stepping
-- @+node:gcross.20100308212437.1395:Lattice
-- @+node:gcross.20100308212437.1397:latticeHasVertex
latticeHasVertex :: Vertex -> LatticeMonad Bool
latticeHasVertex vertex = fmap (Set.member vertex) (gets latticeVertices)
-- @-node:gcross.20100308212437.1397:latticeHasVertex
-- @+node:gcross.20100308212437.1399:addEdgeToLattice
addEdgeToLattice :: Edge -> LatticeMonad ()
addEdgeToLattice edge = modify (\lattice -> lattice { latticeEdges = edge:(latticeEdges lattice) })
-- @-node:gcross.20100308212437.1399:addEdgeToLattice
-- @+node:gcross.20100308212437.1401:addVertexToLattice
addVertexToLattice :: Vertex -> LatticeMonad ()
addVertexToLattice vertex = modify (\lattice -> lattice { latticeVertices = Set.insert vertex (latticeVertices lattice) })
-- @-node:gcross.20100308212437.1401:addVertexToLattice
-- @-node:gcross.20100308212437.1395:Lattice
-- @+node:gcross.20100308212437.1402:Processing Vertices
-- @+node:gcross.20100308212437.1404:processRawVertex
processRawVertex :: [Step] -> RawVertex -> LatticeMonad [RawVertex]
processRawVertex steps raw_vertex = do
    vertex <- lift (resolveVertex raw_vertex)
    has_vertex <- latticeHasVertex vertex
    if has_vertex
        then return []
        else do
            addVertexToLattice vertex
            let stepped_raw_vertices = map (stepFromRawVertex raw_vertex) steps
            stepped_vertices <- lift (mapM resolveVertex stepped_raw_vertices)
            stepped_vertex_ray_numbers <- lift (mapM (findStepNumberForRawVertex steps raw_vertex) stepped_raw_vertices)
            let go queued_raw_vertices
                   ray_number
                   (stepped_vertex:rest_stepped_vertices)
                   (stepped_raw_vertex:rest_stepped_raw_vertices)
                   (stepped_vertex_ray_number:rest_stepped_vertex_ray_numbers)
                  = latticeHasVertex stepped_vertex
                    >>=
                    \has_stepped_vertex ->
                        if has_stepped_vertex
                            then (addEdgeToLattice $
                                    Edge (EdgeSide vertex ray_number)
                                         (EdgeSide stepped_vertex stepped_vertex_ray_number)
                                 )
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
-- @-node:gcross.20100308212437.1404:processRawVertex
-- @+node:gcross.20100308212437.1468:processRawVertices
processRawVertices :: [Step] -> [RawVertex] -> LatticeMonad [RawVertex]
processRawVertices steps = fmap concat . mapM (processRawVertex steps)
-- @-node:gcross.20100308212437.1468:processRawVertices
-- @-node:gcross.20100308212437.1402:Processing Vertices
-- @-node:gcross.20100302164430.1305:Functions
-- @-others
-- @-node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @-leo

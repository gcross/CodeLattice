-- @+leo-ver=4-thin
-- @+node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @@language Haskell

module CodeLattice where

-- @<< Import needed modules >>
-- @+node:gcross.20100302164430.1307:<< Import needed modules >>
import Control.Monad

import Data.EpsilonMatcher.Multiple
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Map as Map
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
-- @+node:gcross.20100302164430.1236:Location
data Location = Location
    {   locationX :: Int
    ,   locationY :: Int
    } deriving (Eq,Ord,Show)

-- @-node:gcross.20100302164430.1236:Location
-- @+node:gcross.20100302164430.1241:Step
data Step = Step
    {   stepAngle :: Double -- in degrees
    ,   stepRotation :: Double -- in degrees
    } deriving (Show)

-- @-node:gcross.20100302164430.1241:Step
-- @+node:gcross.20100302164430.1242:RawVertex
data RawVertex = RawVertex
    {   rawVertexX :: Double
    ,   rawVertexY :: Double
    ,   rawVertexOrientation :: Double
    } deriving (Show)

-- @-node:gcross.20100302164430.1242:RawVertex
-- @+node:gcross.20100302164430.1235:Vertex
data Vertex = Vertex
    {   vertexLocation :: Location
    ,   vertexOrientation :: Angle
    } deriving (Show,Eq)


-- @-node:gcross.20100302164430.1235:Vertex
-- @+node:gcross.20100307163258.1312:Tiling
data Tiling = Tiling
    {   tilingName :: String
    ,   tilingPolygons :: [Int]
    ,   tilingDisambiguation :: Maybe [Int]
    }
-- @-node:gcross.20100307163258.1312:Tiling
-- @+node:gcross.20100302201317.1254:ResolverMonad
type ResolverMonad resultType = MultipleEpsilonMatcherState Double resultType
-- @-node:gcross.20100302201317.1254:ResolverMonad
-- @-node:gcross.20100302164430.1234:Types
-- @+node:gcross.20100302164430.1305:Functions
-- @+node:gcross.20100302201317.1255:module360
modulo360 :: Double -> Double
modulo360 angle = angle - fromIntegral ((floor (angle / 360) :: Int) * 360)
-- @-node:gcross.20100302201317.1255:module360
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
            " connecting "
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
                                    "Step "
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
-- @+node:gcross.20100306220637.1354:runResolverMonad
runResolverMonad :: ResolverMonad resultType -> (resultType,[IntMap Int])
runResolverMonad = runMultipleEpsilonMatchers [1e-5,1e-5,1e-5]
-- @-node:gcross.20100306220637.1354:runResolverMonad
-- @+node:gcross.20100307133316.1314:polygonInteriorAngle
polygonInteriorAngle :: Int -> Double
polygonInteriorAngle n = (n_-2)*180/n_
  where
    n_ = fromIntegral n
-- @-node:gcross.20100307133316.1314:polygonInteriorAngle
-- @+node:gcross.20100307163258.1314:polygonsToEdgesAndAngles
polygonsToEdgesAndAngles :: [Int] -> [((Int,Int),Double)]
polygonsToEdgesAndAngles polygons@(x1:x2:xs) =
    ((last xs,x1),0):go (polygonInteriorAngle x1) polygons
  where
    go angle (x1:x2:xs) = ((x1,x2),angle):go (angle + polygonInteriorAngle x2) (x2:xs)
    go _ _ = []
polygonsToStepAngles _ = error "There needs to be at least two polygons adjoining a vertex!"
-- @-node:gcross.20100307163258.1314:polygonsToEdgesAndAngles
-- @+node:gcross.20100307163258.1319:lookupAngleOfEdge
lookupAngleOfEdge :: [((Int,Int),Double)] -> (Int,Int) -> Int -> Double
lookupAngleOfEdge table edge disambiguation = go table disambiguation
  where
    go :: [((Int,Int),Double)] -> Int -> Double
    go [] _ = error $
        "Error finding "
        ++ show disambiguation ++
        "-th occurance of "
        ++ show edge ++
        " in "
        ++ show table
    go ((edge_to_find,angle):rest) disambiguation
        | edge_to_find == edge
            = if disambiguation == 0
                then angle
                else go rest (disambiguation-1)
        | otherwise
            = go rest disambiguation
-- @-node:gcross.20100307163258.1319:lookupAngleOfEdge
-- @-node:gcross.20100302164430.1305:Functions
-- @+node:gcross.20100307133316.1309:Values
-- @+node:gcross.20100307133316.1310:Tilings
tilings :: [Tiling]
tilings =
    [Tiling "quadrile" [4,4,4,4] Nothing
    ,Tiling "truncated quadrille" [8,8,4] Nothing
    ,Tiling "snub quadrille" [4,3,4,3,3] (Just [0,0,0,1,0])
    ,Tiling "hextille" [6,6,6] Nothing
    ,Tiling "hexadeltille" [6,3,6,3] Nothing
    ,Tiling "truncated hextille" [12,12,3] Nothing
    ,Tiling "deltille" (replicate 6 3) Nothing
    ,Tiling "rhombihexadeltille" [4,6,4,3] Nothing
    ,Tiling "truncated hexadeltille" [12,6,4] Nothing
    ,Tiling "snub hexatille" [6,3,3,3,3] (Just [0,0,1,0,2])
    ,Tiling "isosnub quadrille" [4,4,3,3,3] (Just [0,0,0,0,1])
    ]
-- @-node:gcross.20100307133316.1310:Tilings
-- @-node:gcross.20100307133316.1309:Values
-- @-others
-- @-node:gcross.20100302164430.1233:@thin CodeLattice.hs
-- @-leo

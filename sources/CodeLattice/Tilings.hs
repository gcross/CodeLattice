-- @+leo-ver=4-thin
-- @+node:gcross.20100308112554.1292:@thin Tilings.hs
-- @@language Haskell

module CodeLattice.Tilings where

-- @<< Import needed modules >>
-- @+node:gcross.20100308112554.1293:<< Import needed modules >>
import Data.Maybe

import CodeLattice (Step(..),modulo360)
-- @-node:gcross.20100308112554.1293:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100308112554.1298:Types
-- @+node:gcross.20100308112554.1299:Angle
type Angle = Double
-- @-node:gcross.20100308112554.1299:Angle
-- @+node:gcross.20100308112554.1300:Edge
type Edge = (Int,Int)
-- @-node:gcross.20100308112554.1300:Edge
-- @+node:gcross.20100308112554.1312:EdgesAndAngles
type EdgesAndAngles = [(Edge,Angle)]
-- @-node:gcross.20100308112554.1312:EdgesAndAngles
-- @+node:gcross.20100308112554.1302:Tiling
data Tiling = Tiling
    {   tilingName :: String
    ,   tilingPolygons :: [Int]
    ,   tilingOrientationMode :: OrientationMode
    }
-- @-node:gcross.20100308112554.1302:Tiling
-- @+node:gcross.20100308112554.1315:OrientationMode
data OrientationMode =
    OnlyOneOrientation
  | FixedOrientationRotation Double
  | PickFirstCompatableOrientation
  | PickNthCompatableOrientation [Int]

-- @-node:gcross.20100308112554.1315:OrientationMode
-- @-node:gcross.20100308112554.1298:Types
-- @+node:gcross.20100308112554.1296:Values
-- @+node:gcross.20100308112554.1297:Tilings
tilings :: [Tiling]
tilings =
    [Tiling
        "quadrile"
        [4,4,4,4]
        OnlyOneOrientation
    ,Tiling
        "truncated quadrille"
        [8,8,4]
        PickFirstCompatableOrientation
    ,Tiling
        "snub quadrille"
        [4,3,4,3,3]
        (PickNthCompatableOrientation [0,0,1,1,0])
    ,Tiling
        "hextille"
        [6,6,6]
        (FixedOrientationRotation 180)
    ,Tiling
        "hexadeltille"
        [6,3,6,3]
        (PickNthCompatableOrientation [0,0,1,1])
    ,Tiling
        "truncated hextille"
        [12,12,3]
        PickFirstCompatableOrientation
    ,Tiling
        "deltille"
        (replicate 6 3)
        OnlyOneOrientation
    ,Tiling
        "rhombihexadeltille"
        [4,6,4,3]
        PickFirstCompatableOrientation
-- @+at
--      ,Tiling
--          "truncated hexadeltille"
--          [12,6,4]
--          PickFirstCompatableOrientation
-- @-at
-- @@c
    ,Tiling
        "snub hexatille"
        [6,3,3,3,3]
        (PickNthCompatableOrientation [0,0,1,0,2])
    ,Tiling
        "isosnub quadrille"
        [4,4,3,3,3]
        (PickNthCompatableOrientation [0,0,0,0,1])
    ]
-- @-node:gcross.20100308112554.1297:Tilings
-- @-node:gcross.20100308112554.1296:Values
-- @+node:gcross.20100308112554.1303:Functions
-- @+node:gcross.20100308112554.1305:polygonInteriorAngle
polygonInteriorAngle :: Int -> Double
polygonInteriorAngle n = (n_-2)*180/n_
  where
    n_ = fromIntegral n
-- @-node:gcross.20100308112554.1305:polygonInteriorAngle
-- @+node:gcross.20100308112554.1307:polygonsToEdgesAndAngles
polygonsToEdgesAndAngles :: [Int] -> EdgesAndAngles
polygonsToEdgesAndAngles polygons@(x1:x2:xs) =
    ((last xs,x1),0):go (polygonInteriorAngle x1) polygons
  where
    go angle (x1:x2:xs) = ((x1,x2),angle):go (angle + polygonInteriorAngle x2) (x2:xs)
    go _ _ = []
polygonsToStepAngles _ = error "There needs to be at least two polygons adjoining a vertex!"
-- @-node:gcross.20100308112554.1307:polygonsToEdgesAndAngles
-- @+node:gcross.20100308112554.1309:lookupAngleOfEdge
lookupAngleOfEdge :: EdgesAndAngles -> Edge -> Int -> Double
lookupAngleOfEdge table edge disambiguation = go table disambiguation
  where
    go :: EdgesAndAngles -> Int -> Double
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
-- @-node:gcross.20100308112554.1309:lookupAngleOfEdge
-- @+node:gcross.20100308112554.1311:tilingToSteps
tilingToSteps :: Tiling -> [Step]
tilingToSteps (Tiling _ polygons orientation_mode) =
    case orientation_mode of
        OnlyOneOrientation ->
            map (flip Step 0)
            .
            snd
            .
            unzip
            $
            edges_and_angles
        FixedOrientationRotation rotation ->
            map (flip Step rotation)
            .
            snd
            .
            unzip
            $
            edges_and_angles
        PickFirstCompatableOrientation ->
            [Step angle (modulo360 $ 180 + angle - lookupAngleOfEdge edges_and_angles (p2,p1) 0)
            | ((p1,p2),angle) <- edges_and_angles
            ]
        PickNthCompatableOrientation disambiguations ->
            [Step angle (modulo360 $ 180 + angle - lookupAngleOfEdge edges_and_angles (p2,p1) disambiguation)
            | (((p1,p2),angle),disambiguation) <- zip edges_and_angles disambiguations
            ]
  where
    edges_and_angles = polygonsToEdgesAndAngles polygons
-- @-node:gcross.20100308112554.1311:tilingToSteps
-- @+node:gcross.20100308112554.1314:lookupTiling
lookupTiling :: String -> Tiling
lookupTiling name = go tilings
  where
    go [] = error $ "Unable to find tiling named " ++ name
    go (tiling:rest)
     | (tilingName tiling == name)
        = tiling
     | otherwise
        = go rest
-- @-node:gcross.20100308112554.1314:lookupTiling
-- @-node:gcross.20100308112554.1303:Functions
-- @-others
-- @-node:gcross.20100308112554.1292:@thin Tilings.hs
-- @-leo

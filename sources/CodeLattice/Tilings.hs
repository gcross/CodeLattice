-- @+leo-ver=4-thin
-- @+node:gcross.20100308112554.1292:@thin Tilings.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100713115329.1585:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100713115329.1585:<< Language extensions >>
-- @nl

module CodeLattice.Tilings where

-- @<< Import needed modules >>
-- @+node:gcross.20100308112554.1293:<< Import needed modules >>
import Control.Monad

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Tuple.Select

import CodeLattice hiding (Edge)
import CodeLattice.Discrete
import CodeLattice.Labeling
-- @-node:gcross.20100308112554.1293:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100308112554.1298:Types
-- @+node:gcross.20100308112554.1299:Angle
type Angle = ApproximateDouble
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
    ,   tilingSeedVertex :: Vertex
    ,   tilingPeriodicity :: Periodicity
    ,   tilingNumberOfOrientations :: Int
    ,   tilingNumberOfRays :: Int
    ,   tilingNumberOfSymmetries :: Int
    ,   tilingTranslationSymmetryDistance :: ApproximateDouble
    ,   tilingSteps :: [Step]
    ,   tilingUnitRadiusLattice :: Lattice
    ,   tilingUnitRadiusDiscreteLattice :: DiscreteLattice
    ,   tilingOrientations :: [ApproximateDouble]
    ,   tilingVertexClasses :: VertexClasses
    ,   tilingSymmetries :: [LatticeLabelingPermutation]
    }
-- @nonl
-- @-node:gcross.20100308112554.1302:Tiling
-- @+node:gcross.20100308112554.1315:OrientationMode
data OrientationMode =
    OnlyOneOrientation
  | FixedOrientationRotation ApproximateDouble
  | PickFirstCompatableOrientation
  | PickNthCompatableOrientation [Int]

-- @-node:gcross.20100308112554.1315:OrientationMode
-- @-node:gcross.20100308112554.1298:Types
-- @+node:gcross.20100308112554.1296:Values
-- @+node:gcross.20100308112554.1297:Tilings
tilings :: [Tiling]
tilings =
    [makeTiling
        "quadrille"
        [4,4,4,4]
        OnlyOneOrientation
        (Vertex (-0.5) (-0.5) 0)
        (squarePeriodicity 1)
        1
        1.0
        8
    ,makeTiling
        "truncated quadrille"
        [8,8,4]
        PickFirstCompatableOrientation
        (Vertex (-0.5) (-(0.5 + 1/sqrt 2)) 0)
        (squarePeriodicityRotatedBy 45 (1 + sqrt 2))
        4
        (1 + sqrt 2)
        8
    ,makeTiling
        "snub quadrille"
        [4,3,3,4,3]
        (PickNthCompatableOrientation [0,0,0,1,1])
        (Vertex (-0.5) (-0.5) 0)
        (squarePeriodicityRotatedBy 60
            (sqrt ((1/2+sqrt 2/2*cos (pi/12))^2
                  +(1/2+sqrt 2/2*sin (pi/12))^2
                  )
            )
        )
        4
        (sqrt ((1/2)^2 + (1+sqrt 3/2)^2))
        4
    ,makeTiling
        "hextille"
        [6,6,6]
        (FixedOrientationRotation 180)
        (Vertex (-0.5) (-sqrt 3/2) 0)
        (hexagonalPeriodicityRotatedBy 30 1.5)
        2
        (sqrt 3)
        12
    ,makeTiling
        "hexadeltille"
        [6,3,6,3]
        (PickNthCompatableOrientation [1,1,0,0])
        (Vertex (-0.5) (-sqrt 3/2) 0)
        (hexagonalPeriodicityRotatedBy 30 2)
        3
        2.0
        12
    ,makeTiling
        "truncated hextille"
        [12,12,3]
        PickFirstCompatableOrientation
        (Vertex (-0.5) (-(1 + sqrt 3/2)) 0)
        (hexagonalPeriodicityRotatedBy 30 (2+sqrt 3))
        6
        (2+sqrt 3)
        12
    ,makeTiling
        "deltille"
        (replicate 6 3)
        OnlyOneOrientation
        (Vertex 0 0 0)
        (hexagonalPeriodicityRotatedBy 30 1)
        1
        1.0
        12
    ,makeTiling
        "rhombihexadeltille"
        [3,4,6,4]
        PickFirstCompatableOrientation
        (Vertex (sqrt 3/2) 0.5 0)
        (hexagonalPeriodicityRotatedBy 0 (1.5+sqrt 3/2))
        6
        (1+sqrt 3)
        12
    ,makeTiling
        "isosnub quadrille"
        [4,4,3,3,3]
        (PickNthCompatableOrientation [0,0,0,0,1])
        (Vertex (-0.5) (-0.5) 0)
        (rectangularPeriodicity ((1+sqrt 3/2)/1.5) 1.5)
        2
        1.0
        4
    ]
-- @-node:gcross.20100308112554.1297:Tilings
-- @-node:gcross.20100308112554.1296:Values
-- @+node:gcross.20100308112554.1303:Functions
-- @+node:gcross.20100308112554.1305:polygonInteriorAngle
polygonInteriorAngle :: Int → Angle
polygonInteriorAngle n = (n_-2)*180/n_
  where
    n_ = fromIntegral n
-- @nonl
-- @-node:gcross.20100308112554.1305:polygonInteriorAngle
-- @+node:gcross.20100308112554.1307:polygonsToEdgesAndAngles
polygonsToEdgesAndAngles :: [Int] → EdgesAndAngles
polygonsToEdgesAndAngles polygons@(x1:x2:xs) =
    ((last xs,x1),0):go (polygonInteriorAngle x1) polygons
  where
    go angle (x1:x2:xs) = ((x1,x2),angle):go (angle + polygonInteriorAngle x2) (x2:xs)
    go _ _ = []
polygonsToStepAngles _ = error "There needs to be at least two polygons adjoining a vertex!"
-- @nonl
-- @-node:gcross.20100308112554.1307:polygonsToEdgesAndAngles
-- @+node:gcross.20100308112554.1309:lookupAngleOfEdge
lookupAngleOfEdge :: EdgesAndAngles → Edge → Int → Angle
lookupAngleOfEdge table edge disambiguation = go table disambiguation
  where
    go :: EdgesAndAngles → Int → Angle
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
-- @nonl
-- @-node:gcross.20100308112554.1309:lookupAngleOfEdge
-- @+node:gcross.20100308112554.1311:tilingToSteps
tilingToSteps :: Tiling → [Step]
tilingToSteps Tiling { tilingPolygons = polygons, tilingOrientationMode = orientation_mode } =
    case orientation_mode of
        OnlyOneOrientation →
            map (flip Step 0)
            .
            snd
            .
            unzip
            $
            edges_and_angles
        FixedOrientationRotation rotation →
            map (flip Step rotation)
            .
            snd
            .
            unzip
            $
            edges_and_angles
        PickFirstCompatableOrientation →
            [Step angle (modulo360 $ 180 + angle - lookupAngleOfEdge edges_and_angles (p2,p1) 0)
            | ((p1,p2),angle) ← edges_and_angles
            ]
        PickNthCompatableOrientation disambiguations →
            [Step angle (modulo360 $ 180 + angle - lookupAngleOfEdge edges_and_angles (p2,p1) disambiguation)
            | (((p1,p2),angle),disambiguation) ← zip edges_and_angles disambiguations
            ]
  where
    edges_and_angles = polygonsToEdgesAndAngles polygons
-- @nonl
-- @-node:gcross.20100308112554.1311:tilingToSteps
-- @+node:gcross.20100308112554.1314:lookupTiling
lookupTiling :: String → Tiling
lookupTiling name = go tilings
  where
    go [] = error $ "Unable to find tiling named " ++ name
    go (tiling:rest)
     | (tilingName tiling == name)
        = tiling
     | otherwise
        = go rest
-- @nonl
-- @-node:gcross.20100308112554.1314:lookupTiling
-- @+node:gcross.20100309124842.1400:lookupTilingSteps
lookupTilingSteps :: String → [Step]
lookupTilingSteps = tilingSteps . lookupTiling
-- @nonl
-- @-node:gcross.20100309124842.1400:lookupTilingSteps
-- @+node:gcross.20100309124842.1398:makeTiling
makeTiling ::
    String →
    [Int] →
    OrientationMode →
    Vertex →
    Periodicity →
    Int →
    ApproximateDouble →
    Int →
    Tiling
makeTiling
    name
    polygons
    orientation_mode
    seed_vertex
    periodicity
    number_of_orientations
    translation_symmetry_distance
    number_of_symmetries
    = tiling
  where
    tiling@Tiling{..} =
        Tiling
            name
            polygons
            orientation_mode
            seed_vertex
            periodicity
            number_of_orientations
            (length tilingSteps)
            number_of_symmetries
            translation_symmetry_distance
            (tilingToSteps tiling)
            (generatePeriodicLatticeForTiling tiling 1)
            (discretizeLattice tilingUnitRadiusLattice)
            (latticeOrientations tilingUnitRadiusLattice)
            (computeVertexClassesModifiedBy tiling id)
            (computeTilingSymmetriesAgainst tiling tilingUnitRadiusLattice)
-- @-node:gcross.20100309124842.1398:makeTiling
-- @+node:gcross.20100312175547.1382:runLatticeMonadForTiling
runLatticeMonadForTiling = runLatticeMonad . lookupTilingSteps
-- @-node:gcross.20100312175547.1382:runLatticeMonadForTiling
-- @+node:gcross.20100723201654.1735:computeVertexClassesModifiedBy
computeVertexClassesModifiedBy ::
    Tiling →
    (ApproximateDouble → ApproximateDouble) →
    VertexClasses
computeVertexClassesModifiedBy Tiling{..} f =
    VertexClasses
    .
    map (computeSeedVertexClassModifiedBy . (f .) . (+))
    $
    tilingOrientations
  where
    computeSeedVertexClassModifiedBy f =
        VertexClass
        .
        map (
            modulo360
            .
            f
            .
            stepAngle
        )
        $
        tilingSteps
-- @-node:gcross.20100723201654.1735:computeVertexClassesModifiedBy
-- @+node:gcross.20100723201654.1713:checkTilingSymmetryAgainst
checkTilingSymmetryAgainst ::
    Tiling →
    Lattice →
    (ApproximateDouble → ApproximateDouble) →
    Maybe LatticeLabelingPermutation
checkTilingSymmetryAgainst tiling@Tiling{..} lattice f
  = do  permutation ←
            (tilingVertexClasses ??→??)
            .
            computeVertexClassesModifiedBy tiling
            $
            f
        let Periodicity{..} = tilingPeriodicity
            wrapAround = periodicityWrapVertexAround periodDistance
            borderVertex =
                (== periodDistance)
                .
                periodicityComputeVertexDistance 
            original_vertices = latticeVertices lattice         
            modified_vertices =
                Set.map (\(Vertex x y o) →
                    let r = sqrt (x^2 + y^2)
                        θ = (*(pi/180)) . f . (*(180/pi)) $ atan2 y x
                        new_o =
                            (tilingOrientations !!)
                            .
                            ((
                                map fst
                                .
                                unwrapLatticeLabelingPermutation
                                $
                                permutation
                            ) !!)
                            .
                            fromJust
                            .
                            (`elemIndex` tilingOrientations)
                            $
                            o
                        new_vertex = Vertex (r*cos θ) (r*sin θ) new_o
                    in if borderVertex new_vertex
                        then (new_vertex `min` wrapAround new_vertex)
                        else new_vertex
                ) original_vertices
        if original_vertices == modified_vertices
            then return permutation
            else Nothing
-- @-node:gcross.20100723201654.1713:checkTilingSymmetryAgainst
-- @+node:gcross.20100723201654.1734:computeTilingSymmetriesAgainst
computeTilingSymmetriesAgainst ::
    Tiling →
    Lattice →
    [LatticeLabelingPermutation]
computeTilingSymmetriesAgainst tiling lattice =
    nub
    .
    mapMaybe (checkTilingSymmetryAgainst tiling lattice)
    $
    liftM2 (.)
        (id:map (|⇆) [0,15..90])
        (map (+) [0,15..360])
-- @-node:gcross.20100723201654.1734:computeTilingSymmetriesAgainst
-- @+node:gcross.20100726103932.1756:generatePeriodicLatticeForTiling
generatePeriodicLatticeForTiling :: Tiling → Int → Lattice
generatePeriodicLatticeForTiling Tiling{..} radius =
    sel1
    .
    fst
    .
    runLatticeMonad tilingSteps
    $
    growPeriodicLattice
        tilingPeriodicity
        (+1.0)
        1.0
        radius
        [tilingSeedVertex]
-- @-node:gcross.20100726103932.1756:generatePeriodicLatticeForTiling
-- @-node:gcross.20100308112554.1303:Functions
-- @-others
-- @-node:gcross.20100308112554.1292:@thin Tilings.hs
-- @-leo

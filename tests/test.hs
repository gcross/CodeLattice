-- @+leo-ver=4-thin
-- @+node:gcross.20100302201317.1270:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1411:<< Language extensions >>
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20091217190104.1411:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091217190104.1412:<< Import needed modules >>
import Control.Arrow
import Control.Applicative
import Control.Applicative.Infix
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class

import qualified Data.Bimap as Bimap
import Data.Eq.Approximate
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Tuple.Select

import Debug.Trace

import Test.ChasingBottoms.IsBottom
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import CodeLattice
import CodeLattice.Discrete
import CodeLattice.Labeling
import CodeLattice.Scanning
import CodeLattice.Tilings
-- @-node:gcross.20091217190104.1412:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100714141137.2288:AlmostEq
class AlmostEq a where
    (≈) :: a → a → Bool

instance AlmostEq Double where
    x ≈ y = (abs x + abs y < 1e-11) || (abs (x-y) / abs(x+y) * 2 < 1e-7)

instance (AlmostEq a) => AlmostEq [a] where
    x ≈ y = all (uncurry (≈)) $ zip x y

x /≈ y = not (x ≈ y)
-- @-node:gcross.20100714141137.2288:AlmostEq
-- @+node:gcross.20100715150143.1846:Types
-- @+node:gcross.20100717003017.2447:Bounds
data Bounds = Bounds
    {   boundLeft :: ApproximateDouble
    ,   boundBottom :: ApproximateDouble
    ,   boundRight :: ApproximateDouble
    ,   boundTop :: ApproximateDouble
    } deriving (Show)
-- @-node:gcross.20100717003017.2447:Bounds
-- @+node:gcross.20100715150143.1847:GrownLattice
data GrownLattice = GrownLattice
    {   grownLatticeTilingName :: !String
    ,   grownLattice :: !Lattice
    ,   grownDiscreteLattice :: !DiscreteLattice
    -- ,   grownLatticeSymmetricPermutations :: ![LatticeLabelingPermutation]
    }
-- @-node:gcross.20100715150143.1847:GrownLattice
-- @-node:gcross.20100715150143.1846:Types
-- @+node:gcross.20091217190104.2175:Functions
-- @+node:gcross.20091217190104.2176:echo
echo x = trace (show x) x
-- @-node:gcross.20091217190104.2176:echo
-- @+node:gcross.20091218141305.1337:skipList
skipList :: Int → [a] → [a]
skipList _ [] = []
skipList n (x:xs) = x:skipList n (drop (n-1) xs)
-- @nonl
-- @-node:gcross.20091218141305.1337:skipList
-- @+node:gcross.20100717003017.2448:withinBounds
withinBounds :: Bounds → Vertex → Bool
withinBounds Bounds{..} Vertex{..} =
    (vertexLocationX >= boundLeft) &&
    (vertexLocationX <= boundRight) &&
    (vertexLocationY >= boundTop) &&
    (vertexLocationY <= boundBottom)
-- @-node:gcross.20100717003017.2448:withinBounds
-- @-node:gcross.20091217190104.2175:Functions
-- @+node:gcross.20100307122538.1301:Generators
-- @+node:gcross.20100717003017.2444:ApproximateDouble
instance Arbitrary ApproximateDouble where
    arbitrary = fmap AbsolutelyApproximateValue arbitrary
-- @-node:gcross.20100717003017.2444:ApproximateDouble
-- @+node:gcross.20100307122538.1302:Step
instance Arbitrary Step where
    arbitrary =
        liftM2 Step
            (fmap modulo360 arbitrary)
            (fmap modulo360 arbitrary)
-- @-node:gcross.20100307122538.1302:Step
-- @+node:gcross.20100307133316.1307:Vertex
instance Arbitrary Vertex where
    arbitrary =
        liftM3 Vertex
            arbitrary
            arbitrary
            (fmap modulo360 arbitrary)
-- @-node:gcross.20100307133316.1307:Vertex
-- @-node:gcross.20100307122538.1301:Generators
-- @+node:gcross.20100715150143.1839:Generator functions
-- @+node:gcross.20100715150143.1840:arbitraryVertexLabeling
arbitraryVertexLabeling :: Int → Gen VertexLabeling
arbitraryVertexLabeling number_of_rays =
    fmap (
        canonicalizeVertexLabeling
        .
        VertexLabeling
    )
    .
    vectorOf number_of_rays
    .
    elements
    $
    [1,2,3]
-- @-node:gcross.20100715150143.1840:arbitraryVertexLabeling
-- @+node:gcross.20100715150143.1838:arbitraryLatticeLabeling
arbitraryLatticeLabeling :: Lattice → Gen LatticeLabeling
arbitraryLatticeLabeling lattice =
    fmap LatticeLabeling
    .
    vectorOf (numberOfOrientationsInLattice lattice)
    .
    arbitraryVertexLabeling
    .
    numberOfRaysInLattice
    $
    lattice
-- @-node:gcross.20100715150143.1838:arbitraryLatticeLabeling
-- @-node:gcross.20100715150143.1839:Generator functions
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100302201317.1388:<< Tests >>
    -- @+others
    -- @+node:gcross.20100307133316.1311:Functions
    [testGroup "Functions"
        -- @    @+others
        -- @+node:gcross.20100302201317.1389:modulo360
        [testProperty "modulo360" $
            \(n :: Integer) →
                fromInteger (n `mod` 360) == modulo360 (fromInteger n)
        -- @nonl
        -- @-node:gcross.20100302201317.1389:modulo360
        -- @+node:gcross.20100306220637.1289:stepFromVertex
        ,testGroup "stepFromVertex"
            [   testCase name
                .
                assertEqual
                    "Did the step arrive at the correct vertex?"
                    correct_stepped_vertex
                .
                stepFromVertex originVertex
                $
                step
            | (name,step,correct_stepped_vertex) ←
                [("horizontal",Step 0 0,Vertex 1 0 0)
                ,("vertical",Step 90 0,Vertex 0 1 0)
                ,("horizontal plus rotation",Step 0 30,Vertex 1 0 30)
                ]
            ]
        -- @-node:gcross.20100306220637.1289:stepFromVertex
        -- @+node:gcross.20100307122538.1300:findStepNumberForVertex
        ,testProperty "findStepNumberForVertex" $ do
            NonEmpty steps ← arbitrary
            chosen_step_number ← choose (0,length steps-1)
            let chosen_step = steps !! chosen_step_number
            origin_vertex ← arbitrary
            let vertex_to_find = stepFromVertex origin_vertex chosen_step
                found_step_number =
                    findStepNumberForVertex
                        steps
                        vertex_to_find
                    $
                    origin_vertex
            return (chosen_step_number == found_step_number)
        -- @-node:gcross.20100307122538.1300:findStepNumberForVertex
        -- @+node:gcross.20100307163258.1315:polygonsToEdgesAndAngles
        ,testGroup "polygonsToEdgesAndAngles" $
            [testCase (show polygons) $
                assertEqual
                    "Is the resulting list correct?"
                    correct_list
                    (polygonsToEdgesAndAngles polygons)
            | (polygons,correct_list) ←
                [([4,4,4,4]
                 ,[((4,4),0)
                  ,((4,4),90)
                  ,((4,4),180)
                  ,((4,4),270)
                  ]
                 )
                ,([6,6,6]
                 ,[((6,6),0)
                  ,((6,6),120)
                  ,((6,6),240)
                  ]
                 )
                ,([4,8,8]
                 ,[((8,4),0)
                  ,((4,8),90)
                  ,((8,8),225)
                  ]
                 )
                ,([3,6,3,6]
                 ,[((6,3),0)
                  ,((3,6),60)
                  ,((6,3),180)
                  ,((3,6),240)
                  ]
                 )
                ,([3,3,4,3,4]
                 ,[((4,3),0)
                  ,((3,3),60)
                  ,((3,4),120)
                  ,((4,3),210)
                  ,((3,4),270)
                  ]
                 )
                ]
            ]
        -- @nonl
        -- @-node:gcross.20100307163258.1315:polygonsToEdgesAndAngles
        -- @+node:gcross.20100307163258.1320:lookupAngleOfEdge
        ,testGroup "polygonsToEdgesAndAngles" $
            [testCase (show test_number) $
                assertEqual
                    "Was the correct angle found?"
                    correct_angle
                    (lookupAngleOfEdge (zip edges [0..]) edge_to_find disambiguation)
            | (test_number,(edges,edge_to_find,disambiguation,correct_angle)) ←
                zip [1..]
                [([(0,0)
                  ]
                 ,(0,0)
                 ,0
                 ,0
                 )
                ,([(0,0)
                  ,(0,0)
                  ]
                 ,(0,0)
                 ,1
                 ,1
                 )
                ,([(0,0)
                  ,(1,1)
                  ]
                 ,(1,1)
                 ,0
                 ,1
                 )
                ,([(0,0)
                  ,(2,2)
                  ,(0,0)
                  ,(1,1)
                  ,(0,0)
                  ,(2,2)
                  ]
                 ,(2,2)
                 ,1
                 ,5
                 )
                ,([(0,0)
                  ,(2,2)
                  ,(0,0)
                  ,(1,1)
                  ,(0,0)
                  ,(2,2)
                  ]
                 ,(0,0)
                 ,1
                 ,2
                 )
                ]
            ]
        -- @nonl
        -- @-node:gcross.20100307163258.1320:lookupAngleOfEdge
        -- @+node:gcross.20100309124842.1394:processVertex
        ,testGroup "processVertex"
            -- @    @+others
            -- @+node:gcross.20100309124842.1396:square
            [testGroup "quadrille"
                -- @    @+others
                -- @+node:gcross.20100309124842.1395:1 step
                [testCase "1 step" $ do
                    let (queued_vertices,Lattice{..}) =
                            runLatticeMonadForTiling "quadrille"
                            .
                            processVertex
                            $
                            Vertex 0 0 0
                    assertEqual
                        "Were the correct vertices enqueued?"
                        (Set.fromList
                            [Vertex   0   1  0
                            ,Vertex   1   0  0
                            ,Vertex   0 (-1) 0
                            ,Vertex (-1)  0  0
                            ]
                        )
                        (Set.fromList queued_vertices)
                    assertEqual
                        "Does the lattice have the correct vertices?"
                        (Set.fromList [Vertex 0 0 0])
                        latticeVertices
                    assertEqual
                        "Does the lattice have the correct edges?"
                        []
                        latticeEdges
                -- @-node:gcross.20100309124842.1395:1 step
                -- @+node:gcross.20100309124842.1402:2 steps
                ,testCase "2 step" $ do
                    let (queued_vertices,Lattice{..}) =
                            runLatticeMonadForTiling "quadrille" $
                                processVertex (Vertex 0 0 0)
                                >>=
                                processVertices
                    assertEqual
                        "Were the correct vertices enqueued?"
                        (Set.fromList
                            [Vertex (x+i) (y+j) 0
                            | (x,y) ← [(0,1),(1,0),(0,-1),(-1,0)]
                            , (i,j) ← [(0,1),(1,0),(0,-1),(-1,0)]
                            , (x+i,y+j) /= (0,0)
                            ]
                        )
                        (Set.fromList queued_vertices)

                    assertEqual
                        "Does the lattice have the correct vertices?"
                        (Set.fromList
                            [Vertex   0   0  0
                            ,Vertex (-1)  0  0
                            ,Vertex   0 (-1) 0
                            ,Vertex   1   0  0
                            ,Vertex   0   1  0
                            ]
                        )
                        latticeVertices
                -- @-node:gcross.20100309124842.1402:2 steps
                -- @-others
                ]
            -- @-node:gcross.20100309124842.1396:square
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20100309124842.1394:processVertex
        -- @+node:gcross.20100713003314.1569:canonicalizeLabeling
        ,testGroup "canonicalizeLabeling"
            -- @    @+others
            -- @+node:gcross.20100713003314.1570:null case
            [testCase "null case" $
                assertEqual
                    "Is the canonical labeling correct?"
                    (VertexLabeling [])
                    (canonicalizeVertexLabeling (VertexLabeling []))
            -- @-node:gcross.20100713003314.1570:null case
            -- @+node:gcross.20100713003314.1571:singleton case
            ,testProperty "singleton case" $
                ((== VertexLabeling [1]) . canonicalizeVertexLabeling . VertexLabeling . (:[]))
            -- @-node:gcross.20100713003314.1571:singleton case
            -- @+node:gcross.20100713003314.1572:first element of result is 1
            ,testProperty "first element of result is 1" $
                \(NonEmpty x) →
                    (== 1)
                    .
                    head
                    .
                    unwrapVertexLabeling
                    .
                    canonicalizeVertexLabeling
                    .
                    VertexLabeling
                    $
                    x
            -- @-node:gcross.20100713003314.1572:first element of result is 1
            -- @+node:gcross.20100713003314.1574:test cases
            ,testGroup "first element of result is 1" $
                [testCase (show original_labeling)
                 .
                 assertEqual
                    "Is the canonical labeling correct?"
                    canonical_labeling
                 .
                 unwrapVertexLabeling
                 .
                 canonicalizeVertexLabeling
                 .
                 VertexLabeling
                 $
                 original_labeling
                |(original_labeling,canonical_labeling) ←
                    [([3,3],[1,1])
                    ,([3,2],[1,2])
                    ,([3,3,2,2,3],[1,1,2,2,1])
                    ,([1,1,1,2,3,3,3,2,2],[1,1,1,2,3,3,3,2,2])
                    ,([2,1,3],[1,2,3])
                    ]
                ]
            -- @-node:gcross.20100713003314.1574:test cases
            -- @-others
            ]
        -- @-node:gcross.20100713003314.1569:canonicalizeLabeling
        -- @+node:gcross.20100713115329.1574:generateVertexLabelings
        ,testGroup "generateVertexLabelings"
            -- @    @+others
            -- @+node:gcross.20100713115329.1575:all labelings are canonical
            [testGroup "all labelings are canonical"
                [testCase (show number_of_rays) $
                    mapM_ (\labeling →
                        assertEqual
                            "Does the labeling match the canonical labeling?"
                            (canonicalizeVertexLabeling labeling)
                            labeling
                    )
                    .
                    generateVertexLabelings
                    $
                    number_of_rays
                | number_of_rays ← [1..5]
                ]
            -- @nonl
            -- @-node:gcross.20100713115329.1575:all labelings are canonical
            -- @+node:gcross.20100713115329.1579:all labelings are unique
            ,testGroup "all labelings are unique"
                [testCase (show number_of_rays) $
                    let labelings = generateVertexLabelings number_of_rays
                    in assertEqual
                        "Are the labelings unchanged after removing duplicates?"
                        (nub labelings)
                        labelings
                | number_of_rays ← [1..5]
                ]
            -- @nonl
            -- @-node:gcross.20100713115329.1579:all labelings are unique
            -- @+node:gcross.20100713115329.1577:the correct number of labelings are generated
            ,testGroup "the correct number of labelings are generated"
                [testCase (show number_of_rays) $
                    assertEqual
                        "Was the correct number of labelings generated?"
                        (1 + (3^(number_of_rays-1)-1) `div` 2)
                    .
                    length
                    .
                    generateVertexLabelings
                    $
                    number_of_rays
                | number_of_rays ← [2..5]
                ]
            -- @nonl
            -- @-node:gcross.20100713115329.1577:the correct number of labelings are generated
            -- @+node:gcross.20100713115329.1581:all canonical labelings are generated
            ,testGroup "all canonical labelings are generated"
                [testCase (show number_of_rays) $
                    assertEqual
                        "Were the correct labelings generated?"
                        (sort . nub . map (canonicalizeVertexLabeling . VertexLabeling) $ replicateM number_of_rays [1..3])
                        (sort . generateVertexLabelings $ number_of_rays)
                | number_of_rays ← [1..5]
                ]
            -- @-node:gcross.20100713115329.1581:all canonical labelings are generated
            -- @-others
            ]
        -- @-node:gcross.20100713115329.1574:generateVertexLabelings
        -- @+node:gcross.20100713173607.1602:(|⇆)
        ,testGroup "(reflection operator)"
            -- @    @+others
            -- @+node:gcross.20100713173607.1603:squares to identity
            [testProperty "squares to identity" $
                \reflection_angle →
                    let f = (reflection_angle |⇆)
                    in (f . f) <^(==)^> id
            -- @-node:gcross.20100713173607.1603:squares to identity
            -- @+node:gcross.20100713173607.1610:x then y reflections == rotation by 180
            ,testProperty "x then y reflections == rotation by 180" $
                (modulo360 . (90|⇆) . (0|⇆)) <^(==)^> (>+< 180)
            -- @-node:gcross.20100713173607.1610:x then y reflections == rotation by 180
            -- @-others
            ]
        -- @-node:gcross.20100713173607.1602:(|⇆)
        -- @-others
        ]
    -- @-node:gcross.20100307133316.1311:Functions
    -- @+node:gcross.20100308212437.1385:Ord Vertex
    ,testGroup "Ord Vertex"
        -- @    @+others
        -- @+node:gcross.20100308112554.1321:compare
        [testGroup "compare" $
            -- @    @+others
            -- @+node:gcross.20100308112554.1322:different locations
            [testProperty "different locations" $
                \v1 v2 →
                    (vertexLocation v1 /= vertexLocation v2) ==>
                        (vertexLocation v1 `compare` vertexLocation v2) == (v1 `compare` v2)
            -- @nonl
            -- @-node:gcross.20100308112554.1322:different locations
            -- @+node:gcross.20100308112554.1326:same location
            ,testProperty "same location" $
                \x y o1 o2 →
                    let v1 = Vertex x y o1
                        v2 = Vertex x y o2
                        v1_cmp_v2 = v1 `compare` v2
                    in if o1 == o2
                        then v1_cmp_v2 == EQ
                        else isBottom v1_cmp_v2
            -- @nonl
            -- @-node:gcross.20100308112554.1326:same location
            -- @-others
            ]
        -- @-node:gcross.20100308112554.1321:compare
        -- @+node:gcross.20100308112554.1328:Set.union
        ,testGroup "Set.union" $
            -- @    @+others
            -- @+node:gcross.20100308112554.1329:different locations
            [testProperty "different locations" $
                \v1 v2 →
                    (vertexLocation v1 /= vertexLocation v2) ==>
                        (Set.fromList [v1,v2]) == (Set.fromList [v1] `Set.union` Set.fromList [v2])
            -- @nonl
            -- @-node:gcross.20100308112554.1329:different locations
            -- @+node:gcross.20100308112554.1331:same location
            ,testProperty "same location, same orientation" $
                \x y o1 o2 →
                    let v1 = Vertex x y o1
                        v2 = Vertex x y o2
                        merged_set = Set.fromList [v1] `Set.union` Set.fromList [v2]
                    in if o1 == o2
                        then merged_set == Set.fromList [v1]
                        else isBottom merged_set
            -- @nonl
            -- @-node:gcross.20100308112554.1331:same location
            -- @-others
            ]
        -- @-node:gcross.20100308112554.1328:Set.union
        -- @+node:gcross.20100308112554.1339:Set.member
        ,testGroup "Set.member" $
            -- @    @+others
            -- @+node:gcross.20100308112554.1340:different locations
            [testProperty "different locations" $
                \v1 v2 →
                    (vertexLocation v1 /= vertexLocation v2) ==>
                        Set.notMember v2 (Set.fromList [v1])
            -- @nonl
            -- @-node:gcross.20100308112554.1340:different locations
            -- @+node:gcross.20100308112554.1341:same location
            ,testProperty "same location, same orientation" $
                \x y o1 o2 →
                    let contained = Set.member (Vertex x y o2) (Set.fromList [Vertex x y o1])
                    in if o1 == o2
                        then contained
                        else isBottom contained
            -- @nonl
            -- @-node:gcross.20100308112554.1341:same location
            -- @-others
            ]
        -- @-node:gcross.20100308112554.1339:Set.member
        -- @-others
        ]
    -- @-node:gcross.20100308212437.1385:Ord Vertex
    -- @+node:gcross.20100307133316.1312:Tilings
    ,testGroup "Tilings"
        -- @    @+others
        -- @+node:gcross.20100307133316.1313:sum to 360
        [testGroup "sum to 360" $
            [testCase tilingName $
                assertEqual
                    "Do the interior angles sum to 360?"
                    360
                    (sum . map polygonInteriorAngle $ tilingPolygons)
            | Tiling{..} ← tilings
            ]
        -- @nonl
        -- @-node:gcross.20100307133316.1313:sum to 360
        -- @+node:gcross.20100308112554.1313:correct steps
        ,testGroup "correct steps" $
            [testCase name $
                assertEqual
                    "Do the interior angles sum to 360?"
                    correct_steps
                    (lookupTilingSteps name)
            | (name,correct_steps) ←
                [("quadrille"
                 ,[Step (90 * i) 0 | i ← [0..3]]
                 )
                ,("deltille"
                 ,[Step (60 * i) 0 | i ← [0..5]]
                 )
                ]
            ]
        -- @nonl
        -- @-node:gcross.20100308112554.1313:correct steps
        -- @+node:gcross.20100308112554.1317:invertible steps
        ,testGroup "invertible steps" $
            [testCase (tilingName tiling) $
                forM_ (tilingSteps tiling) $
                    evaluate
                    .
                    findStepNumberForVertex (tilingSteps tiling) originVertex
                    .
                    stepFromVertex originVertex
            | tiling ← tilings
            ]
        -- @-node:gcross.20100308112554.1317:invertible steps
        -- @+node:gcross.20100713173607.1583:ordered steps
        ,testGroup "ordered steps" $
            [testCase (tilingName tiling) $
                let steps = tilingSteps tiling
                in assertEqual
                    "Are the steps ordered?"
                    (sortBy (compare `on` stepAngle) steps)
                    steps
            | tiling ← tilings
            ]
        -- @-node:gcross.20100713173607.1583:ordered steps
        -- @+node:gcross.20100713173607.1586:zero-based steps
        ,testGroup "zero-based steps" $
            [testCase (tilingName tiling) $
                assertEqual
                    "Are the steps ordered?"
                    0
                .
                stepAngle
                .
                head
                .
                tilingSteps
                $
                tiling
            | tiling ← tilings
            ]
        -- @-node:gcross.20100713173607.1586:zero-based steps
        -- @+node:gcross.20100309160622.1349:based on unit radius periodic lattice
        ,testGroup "based on unit radius periodic lattice" $
            -- @    @+others
            -- @+node:gcross.20100309124842.1406:consistent
            [testGroup "consistent" $
                [testCase tilingName
                    .
                    fmap (const ())
                    .
                    evaluate
                    $
                    tilingUnitRadiusLattice
                | Tiling{..} ← tilings
                ]
            -- @-node:gcross.20100309124842.1406:consistent
            -- @+node:gcross.20100309150650.1374:correct number of orientations
            ,testGroup "correct number of orientations" $
                [testCase tilingName $
                    assertEqual
                        "Is the number of computed orientations correct?"
                        tilingNumberOfOrientations
                        .
                        length
                        $
                        tilingOrientations
                | Tiling{..} ← tilings
                ]
            -- @-node:gcross.20100309150650.1374:correct number of orientations
            -- @+node:gcross.20100714141137.2538:correct number of permutations
            ,testGroup "correct number of symmetries" $
                [testCase tilingName $
                    assertEqual
                        "Is the number of symmetries correct?"
                        tilingNumberOfSymmetries
                        .
                        length
                        $
                        tilingSymmetries
                | Tiling{..} ← tilings
                ]
            -- @-node:gcross.20100714141137.2538:correct number of permutations
            -- @+node:gcross.20100309160622.1348:valid adjacencies
            ,testGroup "valid adjacencies" $
                [testCase tilingName
                    .
                    assertEqual
                        "Are there any adjacencies not equal to the number of rays?"
                        []
                    .
                    nub
                    .
                    filter (/= tilingNumberOfRays)
                    .
                    Map.elems
                    .
                    computeVertexAdjacencies
                    $
                    tilingUnitRadiusLattice
                | Tiling{..} ← tilings
                ]
            -- @-node:gcross.20100309160622.1348:valid adjacencies
            -- @+node:gcross.20100723142502.1646:correct lattice translation symmetry distance
            ,testGroup "correct lattice translation symmetry distance" $
                [testCase tilingName
                    .
                    assertEqual
                        "Is the distance correct?"
                        (Just tilingTranslationSymmetryDistance)
                    .
                    latticeTranslationDistance
                    $
                    tilingUnitRadiusLattice
                | Tiling{..} ← tilings
                ]
            -- @-node:gcross.20100723142502.1646:correct lattice translation symmetry distance
            -- @+node:gcross.20100723142502.1647:size of lattice invariant under discretization
            ,testGroup "size of lattice invariant under discretization" $
                [testCase tilingName $ do
                    assertEqual
                        "Has the number of vertices changed?"
                        (Set.size . latticeVertices $ tilingUnitRadiusLattice)
                        (Seq.length . discreteLatticeVertices $ tilingUnitRadiusDiscreteLattice)
                    assertEqual
                        "Has the number of edges changed?"
                        (length . latticeEdges $ tilingUnitRadiusLattice)
                        (length . discreteLatticeEdges $ tilingUnitRadiusDiscreteLattice)
                | Tiling{..} ← tilings
                ]
            -- @-node:gcross.20100723142502.1647:size of lattice invariant under discretization
            -- @+node:gcross.20100723201654.1656:correct number of edges
            ,testGroup "correct number of edges" $
                [testCase tilingName
                    .
                    assertEqual
                        "Is the number of edges correct?"
                        correct_number_of_edges
                    .
                    length
                    .
                    latticeEdges
                    $
                    tilingUnitRadiusLattice
                | (Tiling{..},correct_number_of_edges) ←
                    map (first (\name → fromJust (find ((== name) . tilingName) tilings)))
                        [("quadrille",8)
                        ,("truncated quadrille",24)
                        ,("snub quadrille",20)
                        ,("hextille",9)
                        ,("hexadeltille",24)
                        ,("truncated hextille",36)
                        ,("deltille",12)
                        ,("rhombihexadeltille",36)
                        ,("isosnub quadrille",30)
                        ]
                ]
            -- @-node:gcross.20100723201654.1656:correct number of edges
            -- @+node:gcross.20100723142502.1633:correct pictures
            ,testGroup "correct pictures" $
                [testCase tilingName
                    .
                    assertEqual
                        "Is the picture correct?"
                        correct_picture
                    .
                    drawDiscreteLattice
                    $
                    tilingUnitRadiusDiscreteLattice
                | (Tiling{..},correct_picture) ←
                    map ((\name → fromJust (find ((== name) . tilingName) tilings)) *** unlines)
                        -- @            @+others
                        -- @+node:gcross.20100723142502.1635:quadrille
                        [("quadrille"
                         ,["00"
                          ,"00"
                          ]
                         )
                        -- @-node:gcross.20100723142502.1635:quadrille
                        -- @+node:gcross.20100723142502.1649:truncated quadrille
                        ,("truncated quadrille"
                         ,["  03  "
                          ,"  12  "
                          ,"03  03"
                          ,"12  12"
                          ,"  03  "
                          ,"  12  "
                          ]
                         )
                        -- @-node:gcross.20100723142502.1649:truncated quadrille
                        -- @+node:gcross.20100723142502.1651:snub quadrille
                        ,("snub quadrille"
                         ,["  1  "
                          ," 3 2 "
                          ,"2   0"
                          ," 0 1 "
                          ,"  3  "
                          ]
                         )
                        -- @-node:gcross.20100723142502.1651:snub quadrille
                        -- @+node:gcross.20100723142502.1637:hextille
                        ,("hextille"
                         ,[" 01 "
                          ,"1  0"
                          ," 01 "
                          ]
                         )
                        -- @-node:gcross.20100723142502.1637:hextille
                        -- @+node:gcross.20100723142502.1652:hexadeltille
                        ,("hexadeltille"
                         ,["   1   "
                          ,"0 2 0 2"
                          ," 1   1 "
                          ,"2 0 2 0"
                          ,"   1   "
                          ]
                         )
                        -- @-node:gcross.20100723142502.1652:hexadeltille
                        -- @+node:gcross.20100723142502.1654:truncated hextille
                        ,("truncated hextille"
                         ,["     2     "
                          ,"     5     "
                          ,"3   1 3   1"
                          ," 0 4   0 4 "
                          ,"  2     2  "
                          ,"  5     5  "
                          ," 1 3   1 3 "
                          ,"4   0 4   0"
                          ,"     2     "
                          ,"     5     "
                          ]
                         )
                        -- @-node:gcross.20100723142502.1654:truncated hextille
                        -- @+node:gcross.20100723201654.1661:deltille
                        ,("deltille"
                         ,[" 0 "
                          ,"0 0"
                          ," 0 "
                          ]
                         )
                        -- @-node:gcross.20100723201654.1661:deltille
                        -- @+node:gcross.20100723201654.1663:rhombihexadeltille
                        ,("rhombihexadeltille"
                         ,["   5 3   "
                          ," 4     4 "
                          ,"    1    "
                          ,"0 2   0 2"
                          ,"5 3   5 3"
                          ,"    4    "
                          ," 1     1 "
                          ,"   0 2   "
                          ]
                         )
                        -- @-node:gcross.20100723201654.1663:rhombihexadeltille
                        -- @+node:gcross.20100723201654.1668:isosnub quadrille
                        ,("isosnub quadrille"
                         ,[" 0 0 0"
                          ,"1 1 1 "
                          ,"0 0 0 "
                          ," 1 1 1"
                          ]
                         )
                        -- @-node:gcross.20100723201654.1668:isosnub quadrille
                        -- @-others
                        ]
                ]
            -- @-node:gcross.20100723142502.1633:correct pictures
            -- @-others
            ]
        -- @-node:gcross.20100309160622.1349:based on unit radius periodic lattice
        -- @+node:gcross.20100310140947.1395:correct pictures
        ,testGroup "correct pictures" . const [] $
            -- @    @+others
            -- @+node:gcross.20100310123433.1422:before pruning
            [testGroup "before pruning" $
                [testGroup name $
                    [testCase (show bounds) $
                        assertEqual
                            "Was the drawn picture correct?"
                            (unlines correct_picture)
                            (fst . runLatticeMonadForTiling name $ (
                                growLatticeToBoundsFromOrigin (withinBounds bounds)
                                >>
                                getAndDrawLattice
                            ))
                    | (bounds,correct_picture) ← bounds_and_correct_pictures
                    ]
                | (name,bounds_and_correct_pictures) ←
                    -- @        @+others
                    -- @+node:gcross.20100310123433.1423:quadrille
                    [("quadrille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds 0 0 1 1
                       ,["00"
                        ,"00"
                        ]
                       )
                      ,(Bounds 0 0 2 2
                       ,["000"
                        ,"000"
                        ,"000"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,["000"
                        ,"000"
                        ,"000"
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310123433.1423:quadrille
                    -- @+node:gcross.20100310123433.1425:truncated quadrille
                    ,("truncated quadrille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,["2  "
                        ," 03"
                        ," 12"
                        ]
                       )
                      ,(Bounds (-2) (-1) 1 2
                       ,["03  "
                        ,"12  "
                        ,"  03"
                        ,"  12"
                        ]
                       )
                      ,(Bounds (-2) (-1) 3 4
                       ,["  03  "
                        ,"  12  "
                        ,"03  03"
                        ,"12  12"
                        ,"  03  "
                        ,"  12  "
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310123433.1425:truncated quadrille
                    -- @+node:gcross.20100310140947.1391:snub quadrille
                    ,("snub quadrille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,["  1 "
                        ,"3   "
                        ," 0 2"
                        ,"1   "
                        ,"  3 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 2 2
                       ,["     3   "
                        ," 0 2   0 "
                        ,"     1   "
                        ,"  3     3"
                        ,"2   0 2  "
                        ,"  1     1"
                        ,"     3   "
                        ," 0 2   0 "
                        ,"     1   "
                        ]
                       )
                      ,(Bounds (-3) (-3) 3 3
                       ,["0 2   0 2   0"
                        ,"    1     1  "
                        ," 3     3     "
                        ,"   0 2   0 2 "
                        ," 1     1     "
                        ,"    3     3  "
                        ,"0 2   0 2   0"
                        ,"    1     1  "
                        ," 3     3     "
                        ,"   0 2   0 2 "
                        ," 1     1     "
                        ,"    3     3  "
                        ,"0 2   0 2   0"
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1391:snub quadrille
                    -- @+node:gcross.20100310140947.1384:hextille
                    ,("hextille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,["1  "
                        ," 01"
                        ,"1  "
                        ]
                       )
                      ,(Bounds (-2) (-1) 3 1
                       ,[" 01  01 "
                        ,"1  01  0"
                        ," 01  01 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 3 2
                       ,["1  01  0"
                        ," 01  01 "
                        ,"1  01  0"
                        ," 01  01 "
                        ,"1  01  0"
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1384:hextille
                    -- @+node:gcross.20100310140947.1386:hexdeltille
                    ,("hexadeltille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,[" 1   "
                        ,"2 0 2"
                        ,"   1 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 2 2
                       ,["2 0 2 0 2"
                        ,"   1   1 "
                        ,"0 2 0 2 0"
                        ," 1   1   "
                        ,"2 0 2 0 2"
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1386:hexdeltille
                    -- @+node:gcross.20100310140947.1392:truncated hextille
                    ,("truncated hextille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds 0 (-3) 1 0
                       ,["0 4"
                        ," 2 "
                        ," 5 "
                        ,"1 3"
                        ]
                       )
                      ,(Bounds (-1) (-4) 2 1
                       ,["3   1"
                        ," 0 4 "
                        ,"  2  "
                        ,"  5  "
                        ," 1 3 "
                        ,"4   0"
                        ]
                       )
                      ,(Bounds (-3.5) (-5) 4.5 2
                       ,["   5     5   "
                        ,"  1 3   1 3  "
                        ," 4   0 4   0 "
                        ,"2     2     2"
                        ,"5     5     5"
                        ," 3   1 3   1 "
                        ,"  0 4   0 4  "
                        ,"   2     2   "
                        ]
                       )
                      ]
                     )
                    -- @nonl
                    -- @-node:gcross.20100310140947.1392:truncated hextille
                    -- @+node:gcross.20100310140947.1387:deltille
                    ,("deltille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,[" 0 0 "
                        ,"0 0 0"
                        ," 0 0 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 2 2
                       ,["0 0 0 0 0"
                        ," 0 0 0 0 "
                        ,"0 0 0 0 0"
                        ," 0 0 0 0 "
                        ,"0 0 0 0 0"
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1387:deltille
                    -- @+node:gcross.20100310140947.1393:rhombihexadeltille
                    ,("rhombihexadeltille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,[" 1 3"
                        ," 0 4"
                        ,"5   "
                        ,"  2 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 3 2
                       ,["     5     "
                        ,"  2     2  "
                        ,"3   1 3   1"
                        ,"4   0 4   0"
                        ,"  5     5  "
                        ,"     2     "
                        ," 1 3   1 3 "
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1393:rhombihexadeltille
                    -- @+node:gcross.20100310140947.1394:snub hexatille
                    ,("snub hexatille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,[" 5   "
                        ,"2 0 1"
                        ," 4 3 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 2 2
                       ,["0 1 4 3 5"
                        ," 3 5   2 "
                        ,"  2 0 1 4"
                        ," 1 4 3 5 "
                        ,"3 5   2 0"
                        ]
                       )
                      ]
                     )

                    -- @-node:gcross.20100310140947.1394:snub hexatille
                    -- @+node:gcross.20100310140947.1389:isosnub quadrille
                    ,("isosnub quadrille"
                     ,[(Bounds 0 0 0 0
                       ,["0"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,["1 1 1"
                        ,"0 0 0"
                        ," 1 1 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 2 2
                       ,[" 0 0 0 0 "
                        ,"1 1 1 1 1"
                        ,"0 0 0 0 0"
                        ," 1 1 1 1 "
                        ," 0 0 0 0 "
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1389:isosnub quadrille
                    -- @-others
                    ]
                ]
            -- @nonl
            -- @-node:gcross.20100310123433.1422:before pruning
            -- @+node:gcross.20100310140947.1407:after pruning
            ,testGroup "after pruning" $
                [testGroup name $
                    [testCase (show bounds) $
                        assertEqual
                            "Was the drawn picture correct?"
                            (unlines correct_picture)
                            (fst . runLatticeMonadForTiling name $ (
                                growLatticeToBoundsFromOrigin (withinBounds bounds)
                                >>
                                getAndDrawPrunedLattice
                            ))
                    | (bounds,correct_picture) ← bounds_and_correct_pictures
                    ]
                | (name,bounds_and_correct_pictures) ←
                    -- @        @+others
                    -- @+node:gcross.20100310140947.1408:quadrille
                    [("quadrille"
                     ,[(Bounds 0 0 0 0
                       ,[]
                       )
                      ,(Bounds 0 0 1 1
                       ,["00"
                        ,"00"
                        ]
                       )
                      ,(Bounds 0 0 2 2
                       ,["000"
                        ,"000"
                        ,"000"
                        ]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,["000"
                        ,"000"
                        ,"000"
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1408:quadrille
                    -- @+node:gcross.20100310140947.1409:truncated quadrille
                    ,("truncated quadrille"
                     ,[(Bounds 0 0 0 0
                       ,[]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,["03"
                        ,"12"
                        ]
                       )
                      ,(Bounds (-2) (-1) 1 2
                       ,["03  "
                        ,"12  "
                        ,"  03"
                        ,"  12"
                        ]
                       )
                      ,(Bounds (-2) (-1) 3 4
                       ,["  03  "
                        ,"  12  "
                        ,"03  03"
                        ,"12  12"
                        ,"  03  "
                        ,"  12  "
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1409:truncated quadrille
                    -- @+node:gcross.20100310140947.1411:hextille
                    ,("hextille"
                     ,[(Bounds 0 0 0 0
                       ,[]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,[]
                       )
                      ,(Bounds (-2) (-1) 3 1
                       ,[" 01  01 "
                        ,"1  01  0"
                        ," 01  01 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 3 2
                       ,["   01   "
                        ," 01  01 "
                        ,"1  01  0"
                        ," 01  01 "
                        ,"   01   "
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1411:hextille
                    -- @+node:gcross.20100310140947.1412:hexdeltille
                    ,("hexadeltille"
                     ,[(Bounds 0 0 0 0
                       ,[]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,[" 1   "
                        ,"2 0 2"
                        ,"   1 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 2 2
                       ,["  0 2 0 2"
                        ,"   1   1 "
                        ,"0 2 0 2 0"
                        ," 1   1   "
                        ,"2 0 2 0  "
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1412:hexdeltille
                    -- @+node:gcross.20100310140947.1413:truncated hextille
                    ,("truncated hextille"
                     ,[(Bounds 0 0 0 0
                       ,[]
                       )
                      ,(Bounds 0 (-3) 1 0
                       ,["0 4"
                        ," 2 "
                        ," 5 "
                        ,"1 3"
                        ]
                       )
                      ,(Bounds (-1) (-4) 2 1
                       ,["0 4"
                        ," 2 "
                        ," 5 "
                        ,"1 3"
                        ]
                       )
                      ,(Bounds (-3.5) (-5) 4.5 2
                       ,["   5     5   "
                        ,"  1 3   1 3  "
                        ," 4   0 4   0 "
                        ,"2     2     2"
                        ,"5     5     5"
                        ," 3   1 3   1 "
                        ,"  0 4   0 4  "
                        ,"   2     2   "
                        ]
                       )
                      ]
                     )
                    -- @nonl
                    -- @-node:gcross.20100310140947.1413:truncated hextille
                    -- @+node:gcross.20100310140947.1414:deltille
                    ,("deltille"
                     ,[(Bounds 0 0 0 0
                       ,[]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,[" 0 0 "
                        ,"0 0 0"
                        ," 0 0 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 2 2
                       ,["0 0 0 0 0"
                        ," 0 0 0 0 "
                        ,"0 0 0 0 0"
                        ," 0 0 0 0 "
                        ,"0 0 0 0 0"
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1414:deltille
                    -- @+node:gcross.20100310140947.1415:rhombihexadeltille
                    ,("rhombihexadeltille"
                     ,[(Bounds 0 0 0 0
                       ,[]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,["1 3"
                        ,"0 4"
                        ," 2 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 3 2
                       ,["     5     "
                        ,"  2     2  "
                        ,"3   1 3   1"
                        ,"4   0 4   0"
                        ,"  5     5  "
                        ,"     2     "
                        ," 1 3   1 3 "
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1415:rhombihexadeltille
                    -- @+node:gcross.20100310140947.1416:snub hexatille
                    ,("snub hexatille"
                     ,[(Bounds 0 0 0 0
                       ,[]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,[" 5   "
                        ,"2 0 1"
                        ," 4 3 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 2 2
                       ,["0 1 4 3 5"
                        ," 3 5   2 "
                        ,"  2 0 1 4"
                        ," 1 4 3 5 "
                        ,"3 5   2 0"
                        ]
                       )
                      ]
                     )

                    -- @-node:gcross.20100310140947.1416:snub hexatille
                    -- @+node:gcross.20100310140947.1417:isosnub quadrille
                    ,("isosnub quadrille"
                     ,[(Bounds 0 0 0 0
                       ,[]
                       )
                      ,(Bounds (-1) (-1) 1 1
                       ,["1 1 1"
                        ,"0 0 0"
                        ," 1 1 "
                        ]
                       )
                      ,(Bounds (-2) (-2) 2 2
                       ,[" 0 0 0 0 "
                        ,"1 1 1 1 1"
                        ,"0 0 0 0 0"
                        ," 1 1 1 1 "
                        ," 0 0 0 0 "
                        ]
                       )
                      ]
                     )
                    -- @-node:gcross.20100310140947.1417:isosnub quadrille
                    -- @-others
                    ]
                ]
            -- @nonl
            -- @-node:gcross.20100310140947.1407:after pruning
            -- @-others
            ]
        -- @-node:gcross.20100310140947.1395:correct pictures
        -- @+node:gcross.20100312175547.1383:iterable 8 times
        ,testGroup "iterable 8 times" $
            [testCase tiling_name $ do
                let lattices =
                        sel1
                        .
                        fst
                        .
                        runLatticeMonadForTiling tiling_name
                        .
                        iteratePrunedLattices
                            (liftA2 (max `on` abs) vertexLocationX vertexLocationY)
                            (+2)
                            2
                            [originVertex]
                        $
                        8
                assertEqual
                    "Were the correct number of lattices generated?"
                    8
                    (length lattices)
                forM_ (zip lattices (tail lattices)) $ \(lattice1,lattice2) → do
                    assertBool
                        "Is the number of edges in the lattices monotonically increasing?"
                        (((>) `on` numberOfEdgesInLattice) lattice2 lattice1)
                    assertBool
                        "Is the number of vertices in the lattices monotonically increasing?"
                        (((>) `on` numberOfVerticesInLattice) lattice2 lattice1)
            | tiling_name ← map tilingName tilings
            ]
        -- @nonl
        -- @-node:gcross.20100312175547.1383:iterable 8 times
        -- @-others
        ]
    -- @-node:gcross.20100307133316.1312:Tilings
    -- @+node:gcross.20100715150143.1659:Solving
    ,testGroup "Solving"
        [let scan_configuration = latticeToScanConfiguration lattice
         in testGroup lattice_name
            [testCase (show test_index)
                .
                assertEqual
                    "Was the correct solution obtained by the solver?"
                    correct_solution
                .
                solveForLabeling scan_configuration
                .
                LatticeLabeling
                .
                map VertexLabeling
                $
                labeling
            | test_index ← [1..]
            | (labeling,correct_solution) ← test_cases
            ]
        |(lattice_name,lattice,test_cases) ←
            -- @        @+others
            -- @+node:gcross.20100715150143.1660:4-qubit square, one orientation
            [("4-qubit square, one orientation"
             ,DiscreteLattice
              {   discreteLatticeVertices =
                      Seq.fromList
                          [ DiscreteVertex i j 0
                          | i ← [0,1]
                          , j ← [0,1]
                          ]
              ,   discreteLatticeEdges =
                      [DiscreteEdge (DiscreteEdgeSide 0 0) (DiscreteEdgeSide 2 1)
                      ,DiscreteEdge (DiscreteEdgeSide 0 1) (DiscreteEdgeSide 1 0)
                      ,DiscreteEdge (DiscreteEdgeSide 3 0) (DiscreteEdgeSide 1 1)
                      ,DiscreteEdge (DiscreteEdgeSide 3 1) (DiscreteEdgeSide 2 0)
                      ]
              }
             ,
              -- @  @+others
              -- @+node:gcross.20100715150143.1661:1
              -- @+at
              --  (0,0) X (1,0) X
              --  (0,0) X (0,1) X
              --  (1,1) X (0,1) X
              --  (1,1) X (1,0) X
              -- @-at
              -- @@c
              [([[1,1]],Solution 3 0 1 [1])
              -- @-node:gcross.20100715150143.1661:1
              -- @+node:gcross.20100715150143.1824:2
              -- @+at
              --  (0,0) X (1,0) Z
              --  (0,0) Z (0,1) X
              --  (1,1) X (0,1) Z
              --  (1,1) Z (1,0) X
              -- @-at
              -- @@c
              ,([[1,2]],Solution 2 1 1 [2])
              -- @-node:gcross.20100715150143.1824:2
              -- @-others
              ]
             )
            -- @-node:gcross.20100715150143.1660:4-qubit square, one orientation
            -- @+node:gcross.20100715150143.1828:4-qubit square, two orientations
            ,("4-qubit square, two orientations"
             ,DiscreteLattice
              {   discreteLatticeVertices =
                      Seq.fromList
                          [ DiscreteVertex i j ((i+j) `mod` 2)
                          | i ← [0,1]
                          , j ← [0,1]
                          ]
              ,   discreteLatticeEdges =
                      [DiscreteEdge (DiscreteEdgeSide 0 0) (DiscreteEdgeSide 2 1)
                      ,DiscreteEdge (DiscreteEdgeSide 0 1) (DiscreteEdgeSide 1 0)
                      ,DiscreteEdge (DiscreteEdgeSide 3 0) (DiscreteEdgeSide 1 1)
                      ,DiscreteEdge (DiscreteEdgeSide 3 1) (DiscreteEdgeSide 2 0)
                      ]
              }
             ,
              -- @  @+others
              -- @+node:gcross.20100715150143.1829:1
              -- @+at
              --  (0,0) X (1,0) X
              --  (0,0) X (0,1) X
              --  (1,1) X (0,1) X
              --  (1,1) X (1,0) X
              -- @-at
              -- @@c
              [([[1,1],[1,1]],Solution 3 0 1 [1])
              -- @-node:gcross.20100715150143.1829:1
              -- @+node:gcross.20100715150143.1830:2
              -- @+at
              --  (0,0) X (1,0) Z
              --  (0,0) Z (0,1) X
              --  (1,1) X (0,1) Z
              --  (1,1) Z (1,0) X
              -- @-at
              -- @@c
              ,([[1,2],[1,2]],Solution 2 1 1 [2])
              -- @-node:gcross.20100715150143.1830:2
              -- @+node:gcross.20100715150143.1835:3
              -- @+at
              --  (0,0) X (1,0) X
              --  (0,0) Z (0,1) Z
              --  (1,1) X (0,1) X
              --  (1,1) Z (1,0) Z
              -- @-at
              -- @@c
              ,([[1,2],[2,1]],Solution 2 1 1 [2])
              -- @-node:gcross.20100715150143.1835:3
              -- @+node:gcross.20100715150143.1837:4
              -- @+at
              --  (0,0) X (1,0) X
              --  (0,0) Z (0,1) X
              --  (1,1) X (0,1) X
              --  (1,1) Z (1,0) X
              -- @-at
              -- @@c
              ,([[1,2],[1,1]],Solution 0 2 2 [1,1])
              -- @-node:gcross.20100715150143.1837:4
              -- @-others
              ]
             )
            -- @-node:gcross.20100715150143.1828:4-qubit square, two orientations
            -- @-others
            ]
        ]
    -- @-node:gcross.20100715150143.1659:Solving
    -- @+node:gcross.20100715150143.1843:Symmetries
    ,testGroup "Symmetries" $
        [ testProperty tilingName $
            arbitraryLatticeLabeling tilingUnitRadiusLattice
            >>=
            \labeling → return $
                let (first_solution:rest_solutions) =
                        map (
                            solveForLabeling (latticeToScanConfiguration tilingUnitRadiusDiscreteLattice)
                            .
                            permuteLatticeLabeling labeling
                        ) tilingSymmetries
                in all (== first_solution) rest_solutions
        | Tiling{..} ← tilings
        ]
    -- @-node:gcross.20100715150143.1843:Symmetries
    -- @+node:gcross.20100722123407.1612:Periodicities
    ,testGroup "Periodicities"
        -- @    @+others
        -- @+node:gcross.20100722123407.1613:square
        [testGroup "square" $
            let Periodicity computeVertexDistance wrapVertexAround _ =
                    squarePeriodicityRotatedBy 0 undefined
            in
                -- @        @+others
                -- @+node:gcross.20100723201654.1651:computeVertexDistance
                [testGroup "computeVertexDistance"
                    -- @    @+others
                    -- @+node:gcross.20100722123407.1614:computeVertexDistance
                    [testProperty "correct value" $
                        \vertex@(Vertex x y _) → computeVertexDistance vertex == (max `on` abs) x y
                    -- @-node:gcross.20100722123407.1614:computeVertexDistance
                    -- @+node:gcross.20100723201654.1650:bounded by actual distance
                    ,testProperty "bounded by actual distance" $
                        \x y → computeVertexDistance (Vertex x y undefined) <= sqrt (x^2 + y^2)
                    -- @-node:gcross.20100723201654.1650:bounded by actual distance
                    -- @-others
                    ]
                -- @-node:gcross.20100723201654.1651:computeVertexDistance
                -- @+node:gcross.20100722123407.1618:wrapVertexAround
                ,testGroup "wrapVertexAround"
                    [let vertex = Vertex ax ay 0
                         correct_vertex = Vertex bx by 0
                     in testCase (show (ax,ay) ++ ", d = " ++ show d)
                        .
                        assertEqual
                            "Was the wrapped vertex correct?"
                            correct_vertex
                        .
                        wrapVertexAround d
                        $
                        vertex
                    | ((ax,ay),d,(bx,by)) ←
                        [((0,0),1,(0,0))
                        ,((1,0),1,(-1,0))
                        ,((1,1),1,(-1,-1))
                        ,((2,2),1,(0,0))
                        ,((1.5,2),1,(-0.5,0))
                        ,((2,1.5),1,(0,-0.5))
                        ,((1,1),2,(1,1))
                        ,((3,1),2,(-1,1))
                        ,((2.5,1),2,(-1.5,1))
                        ]
                    ]
                -- @nonl
                -- @-node:gcross.20100722123407.1618:wrapVertexAround
                -- @-others
                ]
        -- @nonl
        -- @-node:gcross.20100722123407.1613:square
        -- @+node:gcross.20100722123407.1624:hexagonal
        ,testGroup "hexagonal" $
            let Periodicity computeVertexDistance wrapVertexAround _ =
                    hexagonalPeriodicityRotatedBy 0 undefined
            in
                -- @        @+others
                -- @+node:gcross.20100722123407.1625:computeVertexDistance
                [testGroup "computeVertexDistance"
                    -- @    @+others
                    -- @+node:gcross.20100722123407.1627:examples
                    [testGroup "examples" $
                        [let vertex = Vertex x y 0
                         in testCase (show (x,y))
                            .
                            assertEqual
                                "Was the distance correct?"
                                correct_distance
                            .
                            computeVertexDistance
                            $
                            vertex
                        | (x,y,correct_distance) ←
                            [(0,1,1)
                            ,(sqrt 3/2,1/2,1)
                            ,(sqrt 3/2,-1/2,1)
                            ,(1,2,2)
                            ,(sqrt 3,0,3/2)
                            ]
                        ]
                    -- @-node:gcross.20100722123407.1627:examples
                    -- @+node:gcross.20100722123407.1628:maximum distance
                    ,testProperty "maximum distance" $
                        \x y →
                            let angle = 180 / pi * atan2 y x
                                correct_distance
                                  | angle < -120 = -(x * sqrt 3 / 2) - (y / 2)
                                  | angle <  -60 = -y
                                  | angle <    0 =  (x * sqrt 3 / 2) - (y / 2)
                                  | angle <   60 =  (x * sqrt 3 / 2) + (y / 2)
                                  | angle <  120 =  y
                                  | angle <  180 = -(x * sqrt 3 / 2) + (y / 2)
                                distance = computeVertexDistance (Vertex x y undefined)
                            in correct_distance == distance
                    -- @-node:gcross.20100722123407.1628:maximum distance
                    -- @+node:gcross.20100723201654.1648:bounded by actual distance
                    ,testProperty "bounded by actual distance" $
                        \x y → computeVertexDistance (Vertex x y undefined) <= sqrt (x^2 + y^2)
                    -- @-node:gcross.20100723201654.1648:bounded by actual distance
                    -- @-others
                    ]
                -- @-node:gcross.20100722123407.1625:computeVertexDistance
                -- @+node:gcross.20100722123407.1630:wrapVertexAround
                ,testGroup "wrapVertexAround"
                    -- @    @+others
                    -- @+node:gcross.20100722123407.1635:examples
                    [testGroup "examples"
                        [let vertex = Vertex ax ay 0
                             correct_vertex = Vertex bx by 0
                         in testCase (show (ax,ay) ++ ", d = " ++ show d)
                            .
                            assertEqual
                                "Was the wrapped vertex correct?"
                                correct_vertex
                            .
                            wrapVertexAround d
                            $
                            vertex
                        | ((ax,ay),d,(bx,by)) ←
                            [((0,1),1,(0,-1))
                            ]
                        ]
                    -- @-node:gcross.20100722123407.1635:examples
                    -- @+node:gcross.20100722123407.1636:expected errors
                    ,testGroup "expected errors"
                        [testProperty (show angle) $
                            \rotation_angle (Positive distance) →
                                let wrapVertexAround =
                                        periodicityWrapVertexAround
                                        .
                                        flip hexagonalPeriodicityRotatedBy undefined
                                        $
                                        (rotation_angle/pi*180)
                                in  isBottom
                                    .
                                    wrapVertexAround distance
                                    $
                                    Vertex
                                        (2 * distance * cos (angle + rotation_angle))
                                        (2 * distance * sin (angle + rotation_angle))
                                        0
                        | angle ← fmap (pi/180*) [0,60..360]
                        ]
                    -- @-node:gcross.20100722123407.1636:expected errors
                    -- @-others
                    ]
                -- @nonl
                -- @-node:gcross.20100722123407.1630:wrapVertexAround
                -- @-others
                ]
        -- @-node:gcross.20100722123407.1624:hexagonal
        -- @+node:gcross.20100723201654.1644:hexagonal (rotated 30 degrees)
        ,testGroup "hexagonal (rotated 30 degrees)" $
            let Periodicity computeVertexDistance wrapVertexAround _ =
                    hexagonalPeriodicityRotatedBy 30 undefined
            in
                -- @        @+others
                -- @+node:gcross.20100723201654.1647:computeVertexDistance
                [testGroup "computeVertexDistance"
                    -- @    @+others
                    -- @+node:gcross.20100723201654.1646:examples
                    [testGroup "examples" $
                        [let vertex = Vertex x y 0
                         in testCase (show (x,y))
                            .
                            assertEqual
                                "Was the distance correct?"
                                correct_distance
                            .
                            computeVertexDistance
                            $
                            vertex
                        | (x,y,correct_distance) ←
                            [(0,2,sqrt 3)
                            ,(1,sqrt 3,2)
                            ]
                        ]
                    -- @-node:gcross.20100723201654.1646:examples
                    -- @+node:gcross.20100723201654.1653:bounded by actual distance
                    ,testProperty "bounded by actual distance" $
                        \x y → computeVertexDistance (Vertex x y undefined) <= sqrt (x^2 + y^2)
                    -- @-node:gcross.20100723201654.1653:bounded by actual distance
                    -- @-others
                    ]
                -- @-node:gcross.20100723201654.1647:computeVertexDistance
                -- @-others
                ]
        -- @-node:gcross.20100723201654.1644:hexagonal (rotated 30 degrees)
        -- @-others
        ]
    -- @-node:gcross.20100722123407.1612:Periodicities
    -- @-others
    -- @-node:gcross.20100302201317.1388:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100302201317.1270:@thin test.hs
-- @-leo

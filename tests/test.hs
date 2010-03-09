-- @+leo-ver=4-thin
-- @+node:gcross.20100302201317.1270:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1411:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
-- @-node:gcross.20091217190104.1411:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091217190104.1412:<< Import needed modules >>
import Control.Exception
import Control.Monad
import Control.Monad.Trans

import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

import Test.ChasingBottoms.IsBottom
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import CodeLattice
import CodeLattice.Tilings
-- @-node:gcross.20091217190104.1412:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091217190104.2175:Functions
-- @+node:gcross.20091217190104.2176:echo
echo x = trace (show x) x
-- @-node:gcross.20091217190104.2176:echo
-- @+node:gcross.20091218141305.1337:skipList
skipList :: Int -> [a] -> [a]
skipList _ [] = []
skipList n (x:xs) = x:skipList n (drop (n-1) xs)
-- @-node:gcross.20091218141305.1337:skipList
-- @-node:gcross.20091217190104.2175:Functions
-- @+node:gcross.20100307122538.1301:Generators
-- @+node:gcross.20100307133316.1308:Location
instance Arbitrary Location where
    arbitrary =
        liftM2 Location
            arbitrary
            arbitrary
-- @-node:gcross.20100307133316.1308:Location
-- @+node:gcross.20100307133316.1307:RawVertex
instance Arbitrary RawVertex where
    arbitrary =
        liftM3 RawVertex
            arbitrary
            arbitrary
            (fmap modulo360 arbitrary)
-- @-node:gcross.20100307133316.1307:RawVertex
-- @+node:gcross.20100307122538.1302:Step
instance Arbitrary Step where
    arbitrary =
        liftM2 Step
            (fmap modulo360 arbitrary)
            (fmap modulo360 arbitrary)
-- @-node:gcross.20100307122538.1302:Step
-- @+node:gcross.20100308112554.1324:Vertex
instance Arbitrary Vertex where
    arbitrary =
        liftM2 Vertex
            arbitrary
            arbitrary
-- @-node:gcross.20100308112554.1324:Vertex
-- @-node:gcross.20100307122538.1301:Generators
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
            \(n :: Integer) ->
                fromInteger (n `mod` 360) == modulo360 (fromInteger n)
        -- @-node:gcross.20100302201317.1389:modulo360
        -- @+node:gcross.20100306220637.1289:stepFromRawVertex
        ,testGroup "stepFromRawVertex"
            -- @    @+others
            -- @+node:gcross.20100306220637.1290:horizontal step
            [testCase "horizontal step" $
                let original_raw_vertex = RawVertex 0 0 0
                    correct_stepped_raw_vertex = RawVertex 1 0 0
                    step = Step 0 0
                    ((stepped_vertex,correct_vertex),_) = runResolverMonad $
                        liftM2 (,)
                            (resolveVertex correct_stepped_raw_vertex)
                            (resolveVertex $ stepFromRawVertex original_raw_vertex step)
                in assertEqual "Did the step arrive at the correct vertex?"
                    correct_vertex
                    stepped_vertex
            -- @-node:gcross.20100306220637.1290:horizontal step
            -- @+node:gcross.20100306220637.1358:vertical step
            ,testCase "vertical step" $
                let original_raw_vertex = RawVertex 0 0 0
                    correct_stepped_raw_vertex = RawVertex 0 1 0
                    step = Step 90 0
                    ((stepped_vertex,correct_vertex),_) = runResolverMonad $
                        liftM2 (,)
                            (resolveVertex correct_stepped_raw_vertex)
                            (resolveVertex $ stepFromRawVertex original_raw_vertex step)
                in assertEqual "Did the step arrive at the correct vertex?"
                    correct_vertex
                    stepped_vertex
            -- @-node:gcross.20100306220637.1358:vertical step
            -- @+node:gcross.20100306220637.1362:step plus rotation
            ,testCase "step plus rotation" $
                let original_raw_vertex = RawVertex 0 0 0
                    correct_stepped_raw_vertex = RawVertex 1 0 30
                    step = Step 0 30
                    ((stepped_vertex,correct_vertex),_) = runResolverMonad $
                        liftM2 (,)
                            (resolveVertex correct_stepped_raw_vertex)
                            (resolveVertex $ stepFromRawVertex original_raw_vertex step)
                in assertEqual "Did the step arrive at the correct vertex?"
                    correct_vertex
                    stepped_vertex
            -- @-node:gcross.20100306220637.1362:step plus rotation
            -- @-others
            ]
        -- @-node:gcross.20100306220637.1289:stepFromRawVertex
        -- @+node:gcross.20100307122538.1300:findStepNumberForRawVertex
        ,testProperty "findStepNumberForRawVertex" $ do
            NonEmpty steps <- arbitrary
            chosen_step_number <- choose (0,length steps-1)
            let chosen_step = steps !! chosen_step_number
            origin_vertex <- arbitrary
            let vertex_to_find = stepFromRawVertex origin_vertex chosen_step
                found_step_number =
                    fst
                    .
                    runResolverMonad
                    .
                    findStepNumberForRawVertex steps vertex_to_find
                    $
                    origin_vertex
            return (chosen_step_number == found_step_number)
        -- @-node:gcross.20100307122538.1300:findStepNumberForRawVertex
        -- @+node:gcross.20100307163258.1315:polygonsToEdgesAndAngles
        ,testGroup "polygonsToEdgesAndAngles" $
            [testCase (show polygons) $
                assertEqual
                    "Is the resulting list correct?"
                    correct_list
                    (polygonsToEdgesAndAngles polygons)
            | (polygons,correct_list) <-
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
            | (test_number,(edges,edge_to_find,disambiguation,correct_angle)) <-
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
        -- @-node:gcross.20100307163258.1320:lookupAngleOfEdge
        -- @+node:gcross.20100309124842.1394:processRawVertex
        ,testGroup "processRawVertex"
            -- @    @+others
            -- @+node:gcross.20100309124842.1396:square
            [testGroup "quadrille"
                -- @    @+others
                -- @+node:gcross.20100309124842.1395:1 step
                [testCase "1 step" $ do
                    let (((queued_vertices,correct_queued_vertices),Lattice vertices edges),_) =
                            runLatticeMonad $
                                liftM2 (,)
                                    (processRawVertex (lookupTilingSteps "quadrille") (RawVertex 0 0 0)
                                     >>=
                                     lift . mapM resolveVertex
                                    )
                                    (lift . mapM resolveVertex $
                                        [RawVertex   0   1  0
                                        ,RawVertex   1   0  0
                                        ,RawVertex   0 (-1) 0
                                        ,RawVertex (-1)  0  0
                                        ]
                                    )

                    assertEqual
                        "Were the correct vertices enqueued?"
                        (Set.fromList correct_queued_vertices)
                        (Set.fromList queued_vertices)
                    assertEqual
                        "Does the lattice have the correct vertices?"
                        (Set.fromList [Vertex (Location 0 0) 0])
                        vertices
                    assertEqual
                        "Does the lattice have the correct edges?"
                        []
                        edges
                -- @-node:gcross.20100309124842.1395:1 step
                -- @+node:gcross.20100309124842.1402:2 steps
                ,testCase "2 step" $ do
                    let (((queued_vertices
                          ,correct_queued_vertices
                          ,correct_vertices
                          )
                         ,Lattice vertices edges
                         ),_) =
                            runLatticeMonad $
                                liftM3 (,,)
                                    (processRawVertex (lookupTilingSteps "quadrille") (RawVertex 0 0 0)
                                     >>=
                                     processRawVertices (lookupTilingSteps "quadrille")
                                     >>=
                                     lift . mapM resolveVertex
                                    )
                                    (lift . mapM resolveVertex $
                                        [RawVertex (x+i) (y+j) 0
                                        | (x,y) <- [(0,1),(1,0),(0,-1),(-1,0)]
                                        , (i,j) <- [(0,1),(1,0),(0,-1),(-1,0)]
                                        , (x+i,y+j) /= (0,0)
                                        ]
                                    )
                                    (lift . mapM resolveVertex $
                                        [RawVertex   0   0  0
                                        ,RawVertex (-1)  0  0
                                        ,RawVertex   0 (-1) 0
                                        ,RawVertex   1   0  0
                                        ,RawVertex   0   1  0
                                        ]
                                    )

                    assertEqual
                        "Were the correct vertices enqueued?"
                        (Set.fromList correct_queued_vertices)
                        (Set.fromList queued_vertices)
                    assertEqual
                        "Does the lattice have the correct vertices?"
                        (Set.fromList correct_vertices)
                        vertices
                    assertEqual
                        "Does the lattice have the correct edges?"
                        (Set.fromList
                            [Edge (EdgeSide (correct_vertices !! (ray_number+1)) ray_number)
                                  (EdgeSide (head correct_vertices) ((ray_number+2) `mod` 4))
                            | ray_number <- [0..3]
                            ]
                        )
                        (Set.fromList edges)
                -- @-node:gcross.20100309124842.1402:2 steps
                -- @-others
                ]
            -- @-node:gcross.20100309124842.1396:square
            -- @-others
            ]
        -- @-node:gcross.20100309124842.1394:processRawVertex
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
                \v1 v2 ->
                    (vertexLocation v1 /= vertexLocation v2) ==>
                        (vertexLocation v1 `compare` vertexLocation v2) == (v1 `compare` v2)
            -- @-node:gcross.20100308112554.1322:different locations
            -- @+node:gcross.20100308112554.1326:same location
            ,testProperty "same location" $
                \l o1 o2 ->
                    let v1 = Vertex l o1
                        v2 = Vertex l o2
                        v1_cmp_v2 = v1 `compare` v2
                    in if o1 == o2
                        then v1_cmp_v2 == EQ
                        else isBottom v1_cmp_v2
            -- @-node:gcross.20100308112554.1326:same location
            -- @-others
            ]
        -- @-node:gcross.20100308112554.1321:compare
        -- @+node:gcross.20100308112554.1328:Set.union
        ,testGroup "Set.union" $
            -- @    @+others
            -- @+node:gcross.20100308112554.1329:different locations
            [testProperty "different locations" $
                \v1 v2 ->
                    (vertexLocation v1 /= vertexLocation v2) ==>
                        (Set.fromList [v1,v2]) == (Set.fromList [v1] `Set.union` Set.fromList [v2])
            -- @-node:gcross.20100308112554.1329:different locations
            -- @+node:gcross.20100308112554.1331:same location
            ,testProperty "same location, same orientation" $
                \l o1 o2 ->
                    let v1 = Vertex l o1
                        v2 = Vertex l o2
                        merged_set = Set.fromList [v1] `Set.union` Set.fromList [v2]
                    in if o1 == o2
                        then merged_set == Set.fromList [v1]
                        else isBottom merged_set
            -- @-node:gcross.20100308112554.1331:same location
            -- @-others
            ]
        -- @-node:gcross.20100308112554.1328:Set.union
        -- @+node:gcross.20100308112554.1339:Set.member
        ,testGroup "Set.member" $
            -- @    @+others
            -- @+node:gcross.20100308112554.1340:different locations
            [testProperty "different locations" $
                \v1 v2 ->
                    (vertexLocation v1 /= vertexLocation v2) ==>
                        Set.notMember v2 (Set.fromList [v1])
            -- @-node:gcross.20100308112554.1340:different locations
            -- @+node:gcross.20100308112554.1341:same location
            ,testProperty "same location, same orientation" $
                \l o1 o2 ->
                    let contained = Set.member (Vertex l o2) (Set.fromList [Vertex l o1])
                    in if o1 == o2
                        then contained
                        else isBottom contained
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
            [testCase name $
                assertEqual
                    "Do the interior angles sum to 360?"
                    360
                    (sum . map polygonInteriorAngle $ polygons)
            | Tiling name polygons _ _ <- tilings
            ]
        -- @-node:gcross.20100307133316.1313:sum to 360
        -- @+node:gcross.20100308112554.1313:correct steps
        ,testGroup "correct steps" $
            [testCase name $
                assertEqual
                    "Do the interior angles sum to 360?"
                    correct_steps
                    (lookupTilingSteps name)
            | (name,correct_steps) <-
                [("quadrille"
                 ,[Step (90 * i) 0 | i <- [0..3]]
                 )
                ,("deltille"
                 ,[Step (60 * i) 0 | i <- [0..5]]
                 )
                ]
            ]
        -- @-node:gcross.20100308112554.1313:correct steps
        -- @+node:gcross.20100308112554.1317:invertible steps
        ,testGroup "invertible steps" $
            [testCase (tilingName tiling) $
                let steps = tilingToSteps tiling
                    origin = RawVertex 0 0 0
                in forM_ steps $
                    evaluate
                    .
                    fst
                    .
                    runResolverMonad
                    .
                    findStepNumberForRawVertex steps origin
                    .
                    stepFromRawVertex origin
            | tiling <- tilings
            ]
        -- @-node:gcross.20100308112554.1317:invertible steps
        -- @-others
        ]
    -- @-node:gcross.20100307133316.1312:Tilings
    -- @-others
    -- @-node:gcross.20100302201317.1388:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100302201317.1270:@thin test.hs
-- @-leo

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

import qualified Data.Bimap as Bimap
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
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
-- @+node:gcross.20100309124842.1410:Grown Lattices
-- @+node:gcross.20100309124842.1411:grown_lattices
grown_lattice_size = 20
grown_lattice_bound = grown_lattice_size / 2

grown_lattices =
    Map.fromList
    [(tiling_name
     ,runLatticeMonadForTiling tiling_name $
        growLatticeToBoundsFromOrigin
            (Bounds
                (-grown_lattice_bound)
                (-grown_lattice_bound)
                grown_lattice_bound
                grown_lattice_bound
             )
     )
    | tiling_name <- map tilingName tilings
    ]
-- @-node:gcross.20100309124842.1411:grown_lattices
-- @+node:gcross.20100309160622.1352:lookupLattice
lookupGrownLattice :: String -> Lattice
lookupGrownLattice =
    snd
    .
    fst
    .
    fromJust
    .
    flip Map.lookup grown_lattices
-- @-node:gcross.20100309160622.1352:lookupLattice
-- @-node:gcross.20100309124842.1410:Grown Lattices
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
                    let (((queued_vertices,correct_queued_vertices),Lattice vertices edges _),_) =
                            runLatticeMonadForTiling "quadrille" $
                                liftM2 (,)
                                    (processRawVertex (RawVertex 0 0 0)
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
                        (Map.keysSet . Bimap.toMapR $ vertices)
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
                         ,Lattice vertices edges _
                         ),_) =
                            runLatticeMonadForTiling "quadrille" $
                                liftM3 (,,)
                                    (processRawVertex (RawVertex 0 0 0)
                                     >>=
                                     processRawVertices
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
                        (Map.keysSet . Bimap.toMapR $ vertices)
                -- @+at
                --      assertEqual
                --          "Does the lattice have the correct edges?"
                --          (Set.fromList
                --              [Edge (EdgeSide (correct_vertices !! 
                --  (ray_number+1)) ray_number)
                --                    (EdgeSide (head correct_vertices) 
                --  ((ray_number+2) `mod` 4))
                --              | ray_number <- [0..3]
                --              ]
                --          )
                --          (Set.fromList edges)
                -- @-at
                -- @@c
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
                forM_ (tilingSteps tiling) $
                    evaluate
                    .
                    fst
                    .
                    runResolverMonad
                    .
                    findStepNumberForRawVertex (tilingSteps tiling) originRawVertex
                    .
                    stepFromRawVertex originRawVertex
            | tiling <- tilings
            ]
        -- @-node:gcross.20100308112554.1317:invertible steps
        -- @+node:gcross.20100309160622.1349:based on grown lattice
        ,testGroup ("based on " ++ show grown_lattice_size ++ "x" ++ show grown_lattice_size ++ " grown lattice") $
            -- @    @+others
            -- @+node:gcross.20100309124842.1406:consistent
            [testGroup "consistent" $
                [testCase name $ do
                    let ((outside_vertices,Lattice vertices edges _),_) =
                            fromJust $
                                Map.lookup name grown_lattices
                    mapM_ evaluate outside_vertices
                    evaluate vertices
                    mapM_ evaluate edges
                | name <- map tilingName tilings
                ]
            -- @-node:gcross.20100309124842.1406:consistent
            -- @+node:gcross.20100309150650.1374:correct number of orientations
            ,testGroup "correct number of orientations" $
                [testCase name $
                    assertEqual
                        "Is the number of computed orientations correct?"
                        correct_number_of_orientations
                        .
                        IntMap.size
                        .
                        (!! 2)
                        .
                        snd
                        .
                        fromJust
                        $
                        Map.lookup name grown_lattices
                | (name,correct_number_of_orientations) <-
                    [("quadrille",1)
                    ,("truncated quadrille",4)
                    ,("snub quadrille",4)
                    ,("hextille",2)
                    ,("hexadeltille",3)
                    ,("truncated hextille",6)
                    ,("deltille",1)
                    ,("rhombihexadeltille",6)
                    -- ,("truncated hexadeltille",12)
                    ,("snub hexatille",6)
                    ,("isosnub quadrille",2)
                    ]
                ]
            -- @-node:gcross.20100309150650.1374:correct number of orientations
            -- @+node:gcross.20100309160622.1348:valid adjacencies
            ,testGroup "valid adjacencies" $
                let checkAdjacenciesOf minimum_count lattice@(Lattice vertices edges _) = do
                        assertEqual
                            "Edges consistent with vertices?"
                            (IntSet.fromList . Bimap.keys $ vertices)
                            (IntMap.keysSet adjacency_map)
                        assertBool
                            ("All vertices adjacent to at least " ++ show (minimum_count+1) ++ " edge(s)?")
                            (IntMap.fold ((&&) . (> minimum_count)) True adjacency_map)
                        assertEqual
                            "Total adjacencies = twice number of edges?"
                            (2 * length edges)
                            (IntMap.fold (+) 0 adjacency_map)
                      where
                        adjacency_map = computeVertexAdjacencies lattice
                in
                    [testGroup "pre-prune" $
                        [testCase name $ checkAdjacenciesOf 0 . lookupGrownLattice $ name
                        | name <- map tilingName tilings
                        ]
                    ,testGroup "post-prune" $
                        [testCase name $ checkAdjacenciesOf 1 . pruneLattice . lookupGrownLattice $ name
                        | name <- map tilingName tilings
                        ]
                    ]
            -- @-node:gcross.20100309160622.1348:valid adjacencies
            -- @-others
            ]
        -- @-node:gcross.20100309160622.1349:based on grown lattice
        -- @+node:gcross.20100310140947.1395:correct pictures
        ,testGroup "correct pictures" $
            -- @    @+others
            -- @+node:gcross.20100310123433.1422:before pruning
            [testGroup "before pruning" $
                [testGroup name $
                    [testCase (show bounds) $
                        assertEqual
                            "Was the drawn picture correct?"
                            (unlines correct_picture)
                            (fst . fst . runLatticeMonadForTiling name $ (
                                growLatticeToBoundsFromOrigin bounds
                                >>
                                getAndDrawLattice
                            ))
                    | (bounds,correct_picture) <- bounds_and_correct_pictures
                    ]
                | (name,bounds_and_correct_pictures) <-
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
                            (fst . fst . runLatticeMonadForTiling name $ (
                                growLatticeToBoundsFromOrigin bounds
                                >>
                                getAndDrawPrunedLattice
                            ))
                    | (bounds,correct_picture) <- bounds_and_correct_pictures
                    ]
                | (name,bounds_and_correct_pictures) <-
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
            -- @-node:gcross.20100310140947.1407:after pruning
            -- @-others
            ]
        -- @-node:gcross.20100310140947.1395:correct pictures
        -- @+node:gcross.20100312175547.1383:iterable 20 times
        ,testGroup "iterable 8 times" $
            [testCase tiling_name $ do
                let lattices =
                        fst
                        .
                        fst
                        .
                        fst
                        .
                        runLatticeMonadForTiling tiling_name
                        .
                        iterateLatticeRepeatedly [originRawVertex]
                        $
                        8
                assertEqual
                    "Were the correct number of lattices generated?"
                    8
                    (length lattices)
                forM_ (zip lattices (tail lattices)) $ \(lattice1,lattice2) -> do
                    assertBool
                        "Is the number of edges in the lattices monotonically increasing?"
                        ((length . latticeEdges) lattice2 > (length . latticeEdges) lattice1)
                    assertBool
                        "Is the number of vertices in the lattices monotonically increasing?"
                        ((Bimap.size . latticeVertices) lattice2 > (Bimap.size . latticeVertices) lattice1)
            | tiling_name <- map tilingName tilings
            ]
        -- @-node:gcross.20100312175547.1383:iterable 20 times
        -- @-others
        ]
    -- @-node:gcross.20100307133316.1312:Tilings
    -- @-others
    -- @-node:gcross.20100302201317.1388:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100302201317.1270:@thin test.hs
-- @-leo

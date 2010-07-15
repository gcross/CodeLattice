-- @+leo-ver=4-thin
-- @+node:gcross.20100302201317.1270:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217190104.1411:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20091217190104.1411:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091217190104.1412:<< Import needed modules >>
import Control.Arrow
import Control.Applicative.Infix
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class

import qualified Data.Bimap as Bimap
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

import Debug.Trace

import Test.ChasingBottoms.IsBottom
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Data.EpsilonMatcher
import Data.EpsilonMatcher.Multiple

import CodeLattice
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
-- @-node:gcross.20091217190104.2175:Functions
-- @+node:gcross.20100309124842.1410:Grown Lattices
-- @+node:gcross.20100309124842.1411:grown_lattices
grown_lattice_size = 20
grown_lattice_bound = grown_lattice_size / 2

grown_lattices =
    Map.fromList
    [(tiling_name
     ,runLatticeMonadForTiling tiling_name $ do
        growLatticeToBoundsFromOrigin
            (Bounds
                (-grown_lattice_bound)
                (-grown_lattice_bound)
                grown_lattice_bound
                grown_lattice_bound
             )
        getAllSymmetricLatticeLabelingPermutations has_reflective_symmetry
     )
    | (tiling_name,has_reflective_symmetry) ← map (tilingName &&& tilingHasReflectiveSymmetry) tilings
    ]
-- @-node:gcross.20100309124842.1411:grown_lattices
-- @+node:gcross.20100309160622.1352:lookupGrownLattice
lookupGrownLattice :: String → Lattice
lookupGrownLattice =
    snd
    .
    fst
    .
    fromJust
    .
    flip Map.lookup grown_lattices
-- @nonl
-- @-node:gcross.20100309160622.1352:lookupGrownLattice
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
    -- @+node:gcross.20100714141137.2510:Epslilon matcher
    [testGroup "Epsilon matcher"
        -- @    @+others
        -- @+node:gcross.20100714141137.2511:Single epsilon matcher
        [testGroup "Single epsilon matcher"
            -- @    @+others
            -- @+node:gcross.20100714141137.2512:Simple integer cases
            [testGroup "Simple integer cases"
                -- @    @+others
                -- @+node:gcross.20100714141137.2513:null case
                [testCase "null case" $
                    assertEqual
                        "Is the match map correct?"
                        ((),IntMap.empty)
                        $
                        runEpsilonMatcher (0 :: Int) $ do
                            return ()
                -- @-node:gcross.20100714141137.2513:null case
                -- @+node:gcross.20100714141137.2514:singleton case
                ,testProperty "singleton case" $ \(value :: Int) →
                    (0,IntMap.singleton 0 0)
                    ==
                    (runEpsilonMatcher (0 :: Int) $ do
                        lookupMatch value
                    )
                -- @nonl
                -- @-node:gcross.20100714141137.2514:singleton case
                -- @+node:gcross.20100714141137.2515:duo case
                ,testProperty "duo case" $ \(value1 :: Int) (value2 :: Int) →
                    (case value1 `compare` value2 of
                        LT → (((0,1),IntMap.fromList [(0,0),(1,1)]) ==)
                        GT → (((0,1),IntMap.fromList [(0,1),(1,0)]) ==)
                        EQ → (((0,0),IntMap.fromList [(0,0)]) ==)
                    )
                    $
                    (runEpsilonMatcher (0 :: Int) $ do
                        key1 ← lookupMatch value1
                        key2 ← lookupMatch value2
                        return (key1,key2)
                    )
                -- @nonl
                -- @-node:gcross.20100714141137.2515:duo case
                -- @+node:gcross.20100714141137.2516:trio case
                ,testProperty "trio case" $ \(value1 :: Int) (value2 :: Int) (value3 :: Int)→
                    (case (value1 `compare` value2,value2 `compare` value3,value1 `compare` value3) of
                        (EQ,EQ,_) → (((0,0,0),IntMap.fromList [(0,0)]) ==)
                        (LT,LT,_) → (((0,1,2),IntMap.fromList [(0,0),(1,1),(2,2)]) ==)
                        (LT,EQ,_) → (((0,1,1),IntMap.fromList [(0,0),(1,1)]) ==)
                        (EQ,LT,_) → (((0,0,1),IntMap.fromList [(0,0),(1,1)]) ==)
                        (GT,GT,_) → (((0,1,2),IntMap.fromList [(0,2),(1,1),(2,0)]) ==)
                        (GT,EQ,_) → (((0,1,1),IntMap.fromList [(0,1),(1,0)]) ==)
                        (EQ,GT,_) → (((0,0,1),IntMap.fromList [(0,1),(1,0)]) ==)
                        (GT,LT,EQ) → (((0,1,0),IntMap.fromList [(0,1),(1,0)]) ==)
                        (GT,LT,LT) → (((0,1,2),IntMap.fromList [(0,1),(1,0),(2,2)]) ==)
                        (GT,LT,GT) → (((0,1,2),IntMap.fromList [(0,2),(1,0),(2,1)]) ==)
                        (LT,GT,EQ) → (((0,1,0),IntMap.fromList [(0,0),(1,1)]) ==)
                        (LT,GT,LT) → (((0,1,2),IntMap.fromList [(0,0),(1,2),(2,1)]) ==)
                        (LT,GT,GT) → (((0,1,2),IntMap.fromList [(0,1),(1,2),(2,0)]) ==)
                    )
                    (runEpsilonMatcher (0 :: Int) $ do
                        key1 ← lookupMatch value1
                        key2 ← lookupMatch value2
                        key3 ← lookupMatch value3
                        return (key1,key2,key3)
                    )
                -- @nonl
                -- @-node:gcross.20100714141137.2516:trio case
                -- @-others
                ]
            -- @-node:gcross.20100714141137.2512:Simple integer cases
            -- @+node:gcross.20100714141137.2517:Simple floating point cases
            ,testGroup "Simple floating point cases"
                -- @    @+others
                -- @+node:gcross.20100714141137.2518:1
                [testCase "1" $
                    assertEqual
                        "Is the result correct?"
                        ((0,1,0,1,2),IntMap.fromList [(0,1),(1,2),(2,0)])
                        (runEpsilonMatcher (0.1 :: Float) $
                            liftM5 (,,,,)
                                (lookupMatch 1.0)
                                (lookupMatch 2.0)
                                (lookupMatch 1.05)
                                (lookupMatch 2.05)
                                (lookupMatch 0.8)
                        )
                -- @-node:gcross.20100714141137.2518:1
                -- @+node:gcross.20100714141137.2519:2
                ,testCase "2" $
                    assertEqual
                        "Is the result correct?"
                        ([0,1,1,2,0,3,4,5],IntMap.fromList [(3,0),(4,1),(1,2),(0,3),(2,4),(5,5)])
                        (runEpsilonMatcher (0.1 :: Float) . sequence $
                            [lookupMatch 1.0
                            ,lookupMatch 0.5
                            ,lookupMatch 0.41
                            ,lookupMatch 1.5
                            ,lookupMatch 1.05
                            ,lookupMatch 0
                            ,lookupMatch 0.2
                            ,lookupMatch 2
                            ]
                        )
                -- @-node:gcross.20100714141137.2519:2
                -- @-others
                ]
            -- @-node:gcross.20100714141137.2517:Simple floating point cases
            -- @-others
            ]
        -- @-node:gcross.20100714141137.2511:Single epsilon matcher
        -- @+node:gcross.20100714141137.2520:Multiple epsilon matchers
        ,testGroup "Multiple epsilon matchers"
            -- @    @+others
            -- @+node:gcross.20100714141137.2521:Simple integer cases
            [testGroup "Simple integer cases"
                -- @    @+others
                -- @+node:gcross.20100714141137.2522:null case
                [testCase "null case" $
                    assertEqual
                        "Is the match map correct?"
                        ((),replicate 3 IntMap.empty)
                        $
                        runMultipleEpsilonMatchers (replicate 3 0 :: [Int]) $ do
                            return ()
                -- @-node:gcross.20100714141137.2522:null case
                -- @+node:gcross.20100714141137.2523:singleton case
                ,testProperty "singleton case" $ \(value1 :: Int) (value2 :: Int) (value3 :: Int) →
                    ([0,0,0],replicate 3 (IntMap.singleton 0 0))
                    ==
                    (runMultipleEpsilonMatchers (replicate 3 0 :: [Int]) . sequence $
                        [lookupMatchIn 0 value1
                        ,lookupMatchIn 1 value2
                        ,lookupMatchIn 2 value3
                        ]
                    )
                -- @nonl
                -- @-node:gcross.20100714141137.2523:singleton case
                -- @-others
                ]
            -- @-node:gcross.20100714141137.2521:Simple integer cases
            -- @+node:gcross.20100714141137.2524:Simple floating point cases
            ,testGroup "Simple floating point cases"
                -- @    @+others
                -- @+node:gcross.20100714141137.2525:1
                [testCase "1" $
                    assertEqual
                        "Is the result correct?"
                        ([0
                         ,0
                         ,0
                         ,0
                         ,1
                         ,1
                         ,1
                         ,2
                         ,0
                         ,1
                         ,3
                         ,4
                         ,5
                         ,0
                         ,0
                         ]
                        ,[IntMap.fromList [(3,0),(4,1),(1,2),(0,3),(2,4),(5,5)]
                         ,IntMap.fromList [(1,0),(0,1)]
                         ,IntMap.fromList [(0,0)]
                         ]
                        )
                        (runMultipleEpsilonMatchers ([0.1,0,0.2] :: [Float]) . sequence $
                            [lookupMatchIn 2 0.4
                            ,lookupMatchIn 0 1.0
                            ,lookupMatchIn 2 0.5
                            ,lookupMatchIn 1 0.5
                            ,lookupMatchIn 0 0.5
                            ,lookupMatchIn 1 0.4
                            ,lookupMatchIn 0 0.41
                            ,lookupMatchIn 0 1.5
                            ,lookupMatchIn 0 1.05
                            ,lookupMatchIn 1 0.4
                            ,lookupMatchIn 0 0
                            ,lookupMatchIn 0 0.2
                            ,lookupMatchIn 0 2
                            ,lookupMatchIn 1 0.5
                            ,lookupMatchIn 2 0.55
                            ]
                        )
                -- @-node:gcross.20100714141137.2525:1
                -- @-others
                ]
            -- @-node:gcross.20100714141137.2524:Simple floating point cases
            -- @-others
            ]
        -- @-node:gcross.20100714141137.2520:Multiple epsilon matchers
        -- @-others
        ]
    -- @-node:gcross.20100714141137.2510:Epslilon matcher
    -- @+node:gcross.20100307133316.1311:Functions
    ,testGroup "Functions"
        -- @    @+others
        -- @+node:gcross.20100302201317.1389:modulo360
        [testProperty "modulo360" $
            \(n :: Integer) →
                fromInteger (n `mod` 360) == modulo360 (fromInteger n)
        -- @nonl
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
            NonEmpty steps ← arbitrary
            chosen_step_number ← choose (0,length steps-1)
            let chosen_step = steps !! chosen_step_number
            origin_vertex ← arbitrary
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
        -- @nonl
        -- @-node:gcross.20100307122538.1300:findStepNumberForRawVertex
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
        -- @+node:gcross.20100309124842.1394:processRawVertex
        ,testGroup "processRawVertex"
            -- @    @+others
            -- @+node:gcross.20100309124842.1396:square
            [testGroup "quadrille"
                -- @    @+others
                -- @+node:gcross.20100309124842.1395:1 step
                [testCase "1 step" $ do
                    let (((queued_vertices,correct_queued_vertices),Lattice vertices edges),_) =
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
                         ,Lattice vertices edges
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
                                        | (x,y) ← [(0,1),(1,0),(0,-1),(-1,0)]
                                        , (i,j) ← [(0,1),(1,0),(0,-1),(-1,0)]
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
                --              | ray_number ← [0..3]
                --              ]
                --          )
                --          (Set.fromList edges)
                -- @-at
                -- @@c
                -- @nonl
                -- @-node:gcross.20100309124842.1402:2 steps
                -- @-others
                ]
            -- @-node:gcross.20100309124842.1396:square
            -- @-others
            ]
        -- @-node:gcross.20100309124842.1394:processRawVertex
        -- @+node:gcross.20100316133702.1476:runThunkOverChoices
        ,testGroup "runThunkOverChoices" $
            let runWith input = do
                    results_ref ← newIORef []
                    runThunkOverChoices (modifyIORef results_ref . (:)) input
                    fmap reverse (readIORef results_ref)
            in
                -- @        @+others
                -- @+node:gcross.20100316133702.1477:[[]]
                [testCase "[[]]" $
                    runWith ([] :: [[()]])
                    >>=
                    assertEqual
                        "Was the correct list generated?"
                        [[]]
                -- @-node:gcross.20100316133702.1477:[[]]
                -- @+node:gcross.20100316133702.1478:[[()]]
                ,testCase "[[()]]" $
                    runWith [[()]]
                    >>=
                    assertEqual
                        "Was the correct list generated?"
                        [[()]]
                -- @-node:gcross.20100316133702.1478:[[()]]
                -- @+node:gcross.20100316133702.1479:[[(),()]]
                ,testCase "[[(),()]]" $
                    runWith [[(),()]]
                    >>=
                    assertEqual
                        "Was the correct list generated?"
                        (replicate 2 [()])
                -- @-node:gcross.20100316133702.1479:[[(),()]]
                -- @+node:gcross.20100316133702.1480:[[()],[()]]
                ,testCase "[[()],[()]]" $
                    runWith [[()],[()]]
                    >>=
                    assertEqual
                        "Was the correct list generated?"
                        [replicate 2 ()]
                -- @-node:gcross.20100316133702.1480:[[()],[()]]
                -- @+node:gcross.20100316133702.1481:[[(),()],[(),()]]
                ,testCase "[[(),()],[(),()]]" $
                    runWith [[(),()],[(),()]]
                    >>=
                    assertEqual
                        "Was the correct list generated?"
                        (replicate 4 . replicate 2 $ ())
                -- @-node:gcross.20100316133702.1481:[[(),()],[(),()]]
                -- @+node:gcross.20100316133702.1482:[[1,2],[3,4]]
                ,testCase "[[1,2],[3,4]]" $
                    runWith [[1,2],[3,4]]
                    >>=
                    assertEqual
                        "Was the correct list generated?"
                        [[1,3]
                        ,[2,3]
                        ,[1,4]
                        ,[2,4]
                        ]
                -- @-node:gcross.20100316133702.1482:[[1,2],[3,4]]
                -- @+node:gcross.20100316133702.1483:replicate 3 [1,2]
                ,testCase "replicate 3 [1,2]" $
                    runWith (replicate 3 [1,2])
                    >>=
                    assertEqual
                        "Was the correct list generated?"
                        [[1,1,1]
                        ,[2,1,1]
                        ,[1,2,1]
                        ,[2,2,1]
                        ,[1,1,2]
                        ,[2,1,2]
                        ,[1,2,2]
                        ,[2,2,2]
                        ]
                -- @-node:gcross.20100316133702.1483:replicate 3 [1,2]
                -- @-others
                ]
        -- @nonl
        -- @-node:gcross.20100316133702.1476:runThunkOverChoices
        -- @+node:gcross.20100331110052.1848:periodizeLatticeGrownWithinRectangularBounds
        ,testGroup "periodizeLatticeGrownWithinRectangularBounds" $
            let arbitraryNonemptyPeriodizedLattice :: Gen Lattice
                arbitraryNonemptyPeriodizedLattice = do
                    tiling ← elements tilings
                    minX ← choose (-10,0)
                    minY ← choose (-10,0)
                    maxX ← choose (0,10)
                    maxY ← choose (0,10)
                    let lattice =
                            unwrapPositionSpaceLattice
                            .
                            periodizeLatticeGrownWithinRectangularBounds
                            $
                            growPositionSpaceLatticeFromTilingToBounds
                                tiling
                                (Bounds minX minY maxX maxY)
                    if isEmptyLattice lattice
                        then arbitraryNonemptyPeriodizedLattice
                        else return lattice
            in
                -- @        @+others
                -- @+node:gcross.20100331110052.1850:Vertex with orientation 0 at origin
                [testProperty "Either empty or has vertex with orientation 0 at origin" $
                    fmap (Bimap.memberR (Vertex (Location 0 0) 0) . latticeVertices) arbitraryNonemptyPeriodizedLattice
                -- @-node:gcross.20100331110052.1850:Vertex with orientation 0 at origin
                -- @+node:gcross.20100331110052.1854:There exists an orientation 0 vertex at every grid point
                ,testProperty "There exists an orientation 0 vertex at every grid point" $
                    fmap (
                        \(Lattice vertices _) →
                            let locations_with_zero_orientation =
                                    map vertexLocation
                                    .
                                    filter ((== 0) . vertexOrientation)
                                    .
                                    Bimap.elems
                                    $
                                    vertices
                                getSetOfCoordinatesAlongBoundary locationA locationB =
                                    mapMaybe (
                                        \location →
                                            if locationA location == 0
                                                then Just (locationB location)
                                                else Nothing
                                        )
                                    $
                                    locations_with_zero_orientation
                                top_boundary_x_values = getSetOfCoordinatesAlongBoundary locationY locationX
                                left_boundary_y_values = getSetOfCoordinatesAlongBoundary locationX locationY
                            in all (\(x,y) → Bimap.memberR (Vertex (Location 0 0) 0) vertices) $
                                zip top_boundary_x_values
                                    left_boundary_y_values
                    ) arbitraryNonemptyPeriodizedLattice
                -- @nonl
                -- @-node:gcross.20100331110052.1854:There exists an orientation 0 vertex at every grid point
                -- @+node:gcross.20100331161014.1557:All vertices have same number of rays
                ,testProperty "All vertices have same number of rays" $
                    fmap (
                        (\(x:xs) → all (==x) xs)
                        .
                        IntMap.elems
                        .
                        computeVertexAdjacencies
                    ) arbitraryNonemptyPeriodizedLattice
                -- @nonl
                -- @-node:gcross.20100331161014.1557:All vertices have same number of rays
                -- @-others
                ]
        -- @nonl
        -- @-node:gcross.20100331110052.1848:periodizeLatticeGrownWithinRectangularBounds
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
                    in (f . f) <^(≈)^> id
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
                \l o1 o2 →
                    let v1 = Vertex l o1
                        v2 = Vertex l o2
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
                \l o1 o2 →
                    let v1 = Vertex l o1
                        v2 = Vertex l o2
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
                \l o1 o2 →
                    let contained = Set.member (Vertex l o2) (Set.fromList [Vertex l o1])
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
            [testCase name $
                assertEqual
                    "Do the interior angles sum to 360?"
                    360
                    (sum . map polygonInteriorAngle $ polygons)
            | Tiling name polygons _ _ _ ← tilings
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
                    fst
                    .
                    runResolverMonad
                    .
                    findStepNumberForRawVertex (tilingSteps tiling) originRawVertex
                    .
                    stepFromRawVertex originRawVertex
            | tiling ← tilings
            ]
        -- @nonl
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
        -- @+node:gcross.20100309160622.1349:based on grown lattice
        ,testGroup ("based on " ++ show grown_lattice_size ++ "x" ++ show grown_lattice_size ++ " grown lattice") $
            -- @    @+others
            -- @+node:gcross.20100309124842.1406:consistent
            [testGroup "consistent" $
                [testCase name $ do
                    let ((outside_vertices,Lattice vertices edges),_) =
                            fromJust $
                                Map.lookup name grown_lattices
                    mapM_ evaluate outside_vertices
                    evaluate vertices
                    mapM_ evaluate edges
                | name ← map tilingName tilings
                ]
            -- @nonl
            -- @-node:gcross.20100309124842.1406:consistent
            -- @+node:gcross.20100309150650.1374:correct number of orientations
            ,testGroup "correct number of orientations" $
                [testCase name $
                    assertEqual
                        "Is the number of computed orientations correct?"
                        correct_number_of_orientations
                        .
                        numberOfOrientationsInLattice
                        .
                        snd
                        .
                        fst
                        .
                        fromJust
                        $
                        Map.lookup name grown_lattices
                | (name,correct_number_of_orientations) ←
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
            -- @nonl
            -- @-node:gcross.20100309150650.1374:correct number of orientations
            -- @+node:gcross.20100714141137.2538:correct number of permutations
            ,testGroup "correct number of symmetric permutations" $
                [testCase name $
                    assertEqual
                        "Is the number of computed symmetric permutations correct?"
                        correct_number_of_symmetric_permutations
                        .
                        length
                        .
                        fst
                        .
                        fst
                        .
                        fromJust
                        $
                        Map.lookup name grown_lattices
                | (name,correct_number_of_symmetric_permutations) ←
                    [("quadrille",8)
                    ,("truncated quadrille",8)
                    ,("snub quadrille",8)
                    ,("hextille",12)
                    ,("hexadeltille",12)
                    ,("truncated hextille",12)
                    ,("deltille",12)
                    ,("rhombihexadeltille",12)
                    -- ,("truncated hexadeltille",12)
                    ,("snub hexatille",6)
                    ,("isosnub quadrille",4)
                    ]
                ]
            -- @-node:gcross.20100714141137.2538:correct number of permutations
            -- @+node:gcross.20100309160622.1348:valid adjacencies
            ,testGroup "valid adjacencies" $
                let checkAdjacenciesOf minimum_count lattice@(Lattice vertices edges) = do
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
                        | name ← map tilingName tilings
                        ]
                    ,testGroup "post-prune" $
                        [testCase name $ checkAdjacenciesOf 1 . pruneLattice . lookupGrownLattice $ name
                        | name ← map tilingName tilings
                        ]
                    ]
            -- @nonl
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
                            (fst . fst . runLatticeMonadForTiling name $ (
                                growLatticeToBoundsFromOrigin bounds
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
                forM_ (zip lattices (tail lattices)) $ \(lattice1,lattice2) → do
                    assertBool
                        "Is the number of edges in the lattices monotonically increasing?"
                        ((length . latticeEdges) lattice2 > (length . latticeEdges) lattice1)
                    assertBool
                        "Is the number of vertices in the lattices monotonically increasing?"
                        ((Bimap.size . latticeVertices) lattice2 > (Bimap.size . latticeVertices) lattice1)
            | tiling_name ← map tilingName tilings
            ]
        -- @nonl
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

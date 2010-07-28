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

instance (AlmostEq a) ⇒ AlmostEq [a] where
    x ≈ y = all (uncurry (≈)) $ zip x y

x /≈ y = not (x ≈ y)
-- @nonl
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
    fmap (decodeVertexLabeling number_of_rays) $
        choose (0,computeNumberOfVertexLabelings number_of_rays-1)
-- @-node:gcross.20100715150143.1840:arbitraryVertexLabeling
-- @+node:gcross.20100727151406.1701:arbitraryLatticeLabeling
arbitraryLatticeLabeling :: Int → Int → Gen LatticeLabeling
arbitraryLatticeLabeling number_of_orientations =
    fmap LatticeLabeling
    .
    vectorOf number_of_orientations
    .
    arbitraryVertexLabeling
-- @-node:gcross.20100727151406.1701:arbitraryLatticeLabeling
-- @+node:gcross.20100715150143.1838:arbitraryLabelingForLattice
arbitraryLabelingForLattice :: Lattice → Gen LatticeLabeling
arbitraryLabelingForLattice =
    liftA2 arbitraryLatticeLabeling
        numberOfOrientationsInLattice
        numberOfRaysInLattice
-- @-node:gcross.20100715150143.1838:arbitraryLabelingForLattice
-- @-node:gcross.20100715150143.1839:Generator functions
-- @-others

main = 
    defaultMain
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
            -- @+node:gcross.20100727151406.1716:the labelings are generated in sorted order
            ,testGroup "the labelings are generated in sorted order"
                [testCase (show number_of_rays) $
                    let labelings = generateVertexLabelings number_of_rays
                    in assertEqual
                        "Were the correct labelings generated?"
                        labelings
                        (sort labelings)
                | number_of_rays ← [1..5]
                ]
            -- @-node:gcross.20100727151406.1716:the labelings are generated in sorted order
            -- @-others
            ]
        -- @-node:gcross.20100713115329.1574:generateVertexLabelings
        -- @+node:gcross.20100727151406.1717:generateLatticeLabelings
        ,testGroup "generateLatticeLabelings"
            -- @    @+others
            -- @+node:gcross.20100727151406.1719:the correct number of labelings are generated
            [testGroup "the correct number of labelings are generated"
                [testCase (show number_of_orientations ++ " x " ++ show number_of_rays) $
                    assertEqual
                        "Was the correct number of labelings generated?"
                        (computeNumberOfLatticeLabelings number_of_orientations number_of_rays)
                    .
                    genericLength
                    .
                    generateLatticeLabelings number_of_orientations
                    $
                    number_of_rays
                | number_of_orientations ← [2..4]
                , number_of_rays ← [2..4]
                , number_of_orientations + number_of_rays <= 6
                ]
            -- @-node:gcross.20100727151406.1719:the correct number of labelings are generated
            -- @+node:gcross.20100727151406.1721:the labelings are generated in sorted order
            ,testGroup "the labelings are generated in sorted order"
                [testCase (show number_of_orientations ++ " x " ++ show number_of_rays) $
                    let labelings = generateLatticeLabelings number_of_orientations number_of_rays
                    in assertEqual
                        "Were the correct labelings generated?"
                        labelings
                        (sort labelings)
                | number_of_orientations ← [2..4]
                , number_of_rays ← [2..4]
                , number_of_orientations + number_of_rays <= 6
                ]
            -- @-node:gcross.20100727151406.1721:the labelings are generated in sorted order
            -- @-others
            ]
        -- @-node:gcross.20100727151406.1717:generateLatticeLabelings
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
        -- @+node:gcross.20100727151406.1676:computeNumberOfVertexLabelings
        ,testGroup "computeNumberOfVertexLabelings"
            [ testCase (show number_of_rays)
                .
                assertEqual
                    "Does computeNumberOfVertexLabelings agree with the length of generateVertexLabelings?"
                    (genericLength . generateVertexLabelings $ number_of_rays)
                .
                computeNumberOfVertexLabelings
                $
                number_of_rays
            | number_of_rays ← [2..6]
            ]
        -- @-node:gcross.20100727151406.1676:computeNumberOfVertexLabelings
        -- @+node:gcross.20100727151406.1682:encodeVertexLabeling
        ,testGroup "encodeVertexLabeling"
            -- @    @+others
            -- @+node:gcross.20100727151406.1683:examples
            [testGroup "examples"
                [ testCase (show example)
                    .
                    assertEqual
                        "Was the encoding correct?"
                        correct_encoding
                    .
                    encodeVertexLabeling
                    .
                    VertexLabeling
                    $
                    example
                | (example,correct_encoding) ←
                    [([1,1],0)
                    ,([1,2],1)
                    ,([1,1,1],0)
                    ,([1,1,2],1)
                    ,([1,2,1],2)
                    ,([1,2,2],3)
                    ,([1,2,3],4)
                    ]
                ]
            -- @-node:gcross.20100727151406.1683:examples
            -- @+node:gcross.20100727151406.1684:encodes to a number less than the number of vertex labelings
            ,testProperty "encodes to a number less than the number of vertex labelings" $ do
                number_of_rays ← choose (2,5)
                labeling ← arbitraryVertexLabeling number_of_rays
                return $
                    encodeVertexLabeling labeling < computeNumberOfVertexLabelings number_of_rays
            -- @-node:gcross.20100727151406.1684:encodes to a number less than the number of vertex labelings
            -- @+node:gcross.20100727151406.1685:matches the sequence of generated labelings
            ,testGroup "matches the sequence of generated labelings"
                [ testCase (show number_of_rays)
                    .
                    assertEqual
                        "Do the encodings match the sequence of generated labelings?"
                        [0..computeNumberOfVertexLabelings number_of_rays-1]
                    .
                    map encodeVertexLabeling
                    .
                    generateVertexLabelings
                    $
                    number_of_rays
                | number_of_rays ← [2..6]
                ]
            -- @-node:gcross.20100727151406.1685:matches the sequence of generated labelings
            -- @+node:gcross.20100727151406.1686:inverse of decoding
            ,testProperty "inverse of decoding" $ do
                number_of_rays ← choose (2,5)
                labeling ← arbitraryVertexLabeling number_of_rays
                return $
                    labeling == (decodeVertexLabeling number_of_rays . encodeVertexLabeling) labeling
            -- @-node:gcross.20100727151406.1686:inverse of decoding
            -- @-others
            ]
        -- @-node:gcross.20100727151406.1682:encodeVertexLabeling
        -- @+node:gcross.20100727151406.1697:encodeLatticeLabeling
        ,testGroup "encodeLatticeLabeling"
            -- @    @+others
            -- @+node:gcross.20100727151406.1705:examples
            [testGroup "examples"
                [ testCase (show example)
                    .
                    assertEqual
                        "Was the encoding correct?"
                        correct_encoding
                    .
                    encodeLatticeLabeling
                    .
                    LatticeLabeling
                    .
                    map VertexLabeling
                    $
                    example
                | (example,correct_encoding) ←
                    [([[1,1]],0)
                    ,([[1,2]],1)
                    ,([[1,1],[1,1]],0)
                    ,([[1,1],[1,2]],1)
                    ,([[1,2],[1,1]],2)
                    ,([[1,2],[1,2]],3)
                    ,([[1,1,1]],0)
                    ,([[1,1,2]],1)
                    ,([[1,2,1]],2)
                    ,([[1,2,2]],3)
                    ,([[1,2,3]],4)
                    ,([[1,2,2],[1,2,1]],3*5+2)
                    ,([[1,1],[1,1],[1,1]],0)
                    ,([[1,1],[1,1],[1,2]],1)
                    ,([[1,1],[1,2],[1,1]],2)
                    ,([[1,1],[1,2],[1,2]],3)
                    ,([[1,2],[1,1],[1,1]],4)
                    ,([[1,2],[1,1],[1,2]],5)
                    ,([[1,2],[1,2],[1,1]],6)
                    ,([[1,2],[1,2],[1,2]],7)
                    ]
                ]
            -- @-node:gcross.20100727151406.1705:examples
            -- @+node:gcross.20100727151406.1699:encodes to a number less than the number of vertex labelings
            ,testProperty "encodes to a number less than the number of lattice labelings" $ do
                number_of_rays ← choose (2,5)
                number_of_orientations ← choose (2,5)
                labeling ← arbitraryLatticeLabeling number_of_orientations number_of_rays
                return $
                    encodeLatticeLabeling labeling < computeNumberOfLatticeLabelings number_of_orientations number_of_rays
            -- @-node:gcross.20100727151406.1699:encodes to a number less than the number of vertex labelings
            -- @+node:gcross.20100727151406.1710:matches the sequence of generated labelings
            ,testGroup "matches the sequence of generated labelings"
                [ testCase (show number_of_orientations ++ " x " ++ show number_of_rays)
                    .
                    assertEqual
                        "Do the encodings match the sequence of generated labelings?"
                        [0..computeNumberOfLatticeLabelings number_of_orientations number_of_rays-1]
                    .
                    map encodeLatticeLabeling
                    .
                    generateLatticeLabelings number_of_orientations
                    $
                    number_of_rays
                | number_of_orientations ← [1..4]
                , number_of_rays ← [2..4]
                , number_of_orientations + number_of_rays <= 6
                ]
            -- @-node:gcross.20100727151406.1710:matches the sequence of generated labelings
            -- @+node:gcross.20100727151406.1703:inverse of decoding
            ,testProperty "inverse of decoding" $ do
                number_of_rays ← choose (2,5)
                number_of_orientations ← choose (2,5)
                labeling ← arbitraryLatticeLabeling number_of_orientations number_of_rays
                return $
                    labeling == (decodeLatticeLabeling number_of_orientations number_of_rays . encodeLatticeLabeling) labeling
            -- @-node:gcross.20100727151406.1703:inverse of decoding
            -- @-others
            ]
        -- @-node:gcross.20100727151406.1697:encodeLatticeLabeling
        -- @+node:gcross.20100727151406.1692:decodeVertexLabeling
        ,testGroup "decodeVertexLabeling"
            -- @    @+others
            -- @+node:gcross.20100727151406.1693:examples
            [testGroup "examples"
                [ testCase (show example)
                    .
                    assertEqual
                        "Was the decoding correct?"
                        (VertexLabeling correct_decoding)
                    .
                    decodeVertexLabeling (length correct_decoding)
                    $
                    example
                | (correct_decoding,example) ←
                    [([1,1],0)
                    ,([1,2],1)
                    ,([1,1,1],0)
                    ,([1,1,2],1)
                    ,([1,2,1],2)
                    ,([1,2,2],3)
                    ,([1,2,3],4)
                    ]
                ]
            -- @-node:gcross.20100727151406.1693:examples
            -- @+node:gcross.20100727151406.1694:all decodings result in a canonical labeling
            ,testProperty "all decodings result in a canonical labeling" $ do
                number_of_rays ← choose (2,5)
                encoding ← choose (0,computeNumberOfVertexLabelings number_of_rays-1)
                let labeling = decodeVertexLabeling number_of_rays encoding
                return $
                    labeling == canonicalizeVertexLabeling labeling
            -- @-node:gcross.20100727151406.1694:all decodings result in a canonical labeling
            -- @+node:gcross.20100727151406.1695:matches the sequence of generated labelings
            ,testGroup "matches the sequence of generated labelings"
                [ testCase (show number_of_rays)
                    .
                    assertEqual
                        "Do the encodings match the sequence of generated labelings?"
                        (generateVertexLabelings number_of_rays)
                    .
                    map (decodeVertexLabeling number_of_rays)
                    $
                    [0..computeNumberOfVertexLabelings number_of_rays-1]
                | number_of_rays ← [2..6]
                ]
            -- @-node:gcross.20100727151406.1695:matches the sequence of generated labelings
            -- @+node:gcross.20100727151406.1696:inverse of encoding
            ,testProperty "inverse of encoding" $ do
                number_of_rays ← choose (2,5)
                encoding ← choose (0,computeNumberOfVertexLabelings number_of_rays-1)
                return $
                    encoding == (encodeVertexLabeling . decodeVertexLabeling number_of_rays) encoding
            -- @-node:gcross.20100727151406.1696:inverse of encoding
            -- @-others
            ]
        -- @-node:gcross.20100727151406.1692:decodeVertexLabeling
        -- @+node:gcross.20100727151406.1706:decodeLatticeLabeling
        ,testGroup "decodeLatticeLabeling"
            -- @    @+others
            -- @+node:gcross.20100727151406.1708:examples
            [testGroup "examples"
                [ testCase (show example)
                    .
                    assertEqual
                        "Was the decoding correct?"
                        correct_decoding
                    .
                    map unwrapVertexLabeling
                    .
                    unwrapLatticeLabeling
                    .
                    decodeLatticeLabeling (length correct_decoding) ((length . head) correct_decoding)
                    $
                    example
                | (correct_decoding,example) ←
                    [([[1,1]],0)
                    ,([[1,2]],1)
                    ,([[1,1],[1,1]],0)
                    ,([[1,1],[1,2]],1)
                    ,([[1,2],[1,1]],2)
                    ,([[1,2],[1,2]],3)
                    ,([[1,1,1]],0)
                    ,([[1,1,2]],1)
                    ,([[1,2,1]],2)
                    ,([[1,2,2]],3)
                    ,([[1,2,3]],4)
                    ,([[1,2,2],[1,2,1]],3*5+2)
                    ,([[1,1],[1,1],[1,1]],0)
                    ,([[1,1],[1,1],[1,2]],1)
                    ,([[1,1],[1,2],[1,1]],2)
                    ,([[1,1],[1,2],[1,2]],3)
                    ,([[1,2],[1,1],[1,1]],4)
                    ,([[1,2],[1,1],[1,2]],5)
                    ,([[1,2],[1,2],[1,1]],6)
                    ,([[1,2],[1,2],[1,2]],7)
                    ]
                ]
            -- @-node:gcross.20100727151406.1708:examples
            -- @+node:gcross.20100727151406.1713:matches the sequence of generated labelings
            ,testGroup "matches the sequence of generated labelings"
                [ testCase (show number_of_orientations ++ " x " ++ show number_of_rays)
                    .
                    assertEqual
                        "Do the decodings match the sequence of generated labelings?"
                        (generateLatticeLabelings number_of_orientations number_of_rays)
                    .
                    map (decodeLatticeLabeling number_of_orientations number_of_rays)
                    $
                    [0..computeNumberOfLatticeLabelings number_of_orientations number_of_rays-1]
                | number_of_orientations ← [1..4]
                , number_of_rays ← [2..4]
                , number_of_orientations + number_of_rays <= 6
                ]
            -- @-node:gcross.20100727151406.1713:matches the sequence of generated labelings
            -- @+node:gcross.20100727151406.1715:inverse of encoding
            ,testProperty "inverse of encoding" $ do
                number_of_orientations ← choose (2,5)
                number_of_rays ← choose (2,5)
                encoding ← choose (0,computeNumberOfLatticeLabelings number_of_orientations number_of_rays-1)
                return $
                    encoding == (encodeLatticeLabeling . decodeLatticeLabeling number_of_orientations number_of_rays) encoding
            -- @-node:gcross.20100727151406.1715:inverse of encoding
            -- @-others
            ]
        -- @-node:gcross.20100727151406.1706:decodeLatticeLabeling
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
    -- @+node:gcross.20100726103932.1673:Solving
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
            -- @+node:gcross.20100726103932.1674:4-qubit square, one orientation
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
              -- @+node:gcross.20100726103932.1675:1
              -- @+at
              --  (0,0) X (1,0) X
              --  (0,0) X (0,1) X
              --  (1,1) X (0,1) X
              --  (1,1) X (1,0) X
              -- @-at
              -- @@c
              [([[1,1]],Solution 3 0 1 [1])
              -- @-node:gcross.20100726103932.1675:1
              -- @+node:gcross.20100726103932.1676:2
              -- @+at
              --  (0,0) X (1,0) Z
              --  (0,0) Z (0,1) X
              --  (1,1) X (0,1) Z
              --  (1,1) Z (1,0) X
              -- @-at
              -- @@c
              ,([[1,2]],Solution 2 1 1 [2])
              -- @-node:gcross.20100726103932.1676:2
              -- @-others
              ]
             )
            -- @-node:gcross.20100726103932.1674:4-qubit square, one orientation
            -- @+node:gcross.20100726103932.1677:4-qubit square, two orientations
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
              -- @+node:gcross.20100726103932.1678:1
              -- @+at
              --  (0,0) X (1,0) X
              --  (0,0) X (0,1) X
              --  (1,1) X (0,1) X
              --  (1,1) X (1,0) X
              -- @-at
              -- @@c
              [([[1,1],[1,1]],Solution 3 0 1 [1])
              -- @-node:gcross.20100726103932.1678:1
              -- @+node:gcross.20100726103932.1679:2
              -- @+at
              --  (0,0) X (1,0) Z
              --  (0,0) Z (0,1) X
              --  (1,1) X (0,1) Z
              --  (1,1) Z (1,0) X
              -- @-at
              -- @@c
              ,([[1,2],[1,2]],Solution 2 1 1 [2])
              -- @-node:gcross.20100726103932.1679:2
              -- @+node:gcross.20100726103932.1680:3
              -- @+at
              --  (0,0) X (1,0) X
              --  (0,0) Z (0,1) Z
              --  (1,1) X (0,1) X
              --  (1,1) Z (1,0) Z
              -- @-at
              -- @@c
              ,([[1,2],[2,1]],Solution 2 1 1 [2])
              -- @-node:gcross.20100726103932.1680:3
              -- @+node:gcross.20100726103932.1681:4
              -- @+at
              --  (0,0) X (1,0) X
              --  (0,0) Z (0,1) X
              --  (1,1) X (0,1) X
              --  (1,1) Z (1,0) X
              -- @-at
              -- @@c
              ,([[1,2],[1,1]],Solution 0 2 2 [1,1])
              -- @-node:gcross.20100726103932.1681:4
              -- @-others
              ]
             )
            -- @-node:gcross.20100726103932.1677:4-qubit square, two orientations
            -- @-others
            ]
        ]
    -- @-node:gcross.20100726103932.1673:Solving
    -- @+node:gcross.20100726103932.1700:Periodicities
    ,testGroup "Periodicities"
        -- @    @+others
        -- @+node:gcross.20100726103932.1701:square
        [testGroup "square" $
            let Periodicity computeVertexDistance wrapVertexAround _ =
                    squarePeriodicityRotatedBy 0 undefined
            in
                -- @        @+others
                -- @+node:gcross.20100726103932.1702:computeVertexDistance
                [testGroup "computeVertexDistance"
                    -- @    @+others
                    -- @+node:gcross.20100726103932.1703:computeVertexDistance
                    [testProperty "correct value" $
                        \vertex@(Vertex x y _) → computeVertexDistance vertex == (max `on` abs) x y
                    -- @-node:gcross.20100726103932.1703:computeVertexDistance
                    -- @+node:gcross.20100726103932.1704:bounded by actual distance
                    ,testProperty "bounded by actual distance" $
                        \x y → computeVertexDistance (Vertex x y undefined) <= sqrt (x^2 + y^2)
                    -- @-node:gcross.20100726103932.1704:bounded by actual distance
                    -- @-others
                    ]
                -- @-node:gcross.20100726103932.1702:computeVertexDistance
                -- @+node:gcross.20100726103932.1705:wrapVertexAround
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
                -- @-node:gcross.20100726103932.1705:wrapVertexAround
                -- @-others
                ]
        -- @nonl
        -- @-node:gcross.20100726103932.1701:square
        -- @+node:gcross.20100726103932.1706:hexagonal
        ,testGroup "hexagonal" $
            let Periodicity computeVertexDistance wrapVertexAround _ =
                    hexagonalPeriodicityRotatedBy 0 undefined
            in
                -- @        @+others
                -- @+node:gcross.20100726103932.1707:computeVertexDistance
                [testGroup "computeVertexDistance"
                    -- @    @+others
                    -- @+node:gcross.20100726103932.1708:examples
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
                    -- @-node:gcross.20100726103932.1708:examples
                    -- @+node:gcross.20100726103932.1709:maximum distance
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
                    -- @-node:gcross.20100726103932.1709:maximum distance
                    -- @+node:gcross.20100726103932.1710:bounded by actual distance
                    ,testProperty "bounded by actual distance" $
                        \x y → computeVertexDistance (Vertex x y undefined) <= sqrt (x^2 + y^2)
                    -- @-node:gcross.20100726103932.1710:bounded by actual distance
                    -- @-others
                    ]
                -- @-node:gcross.20100726103932.1707:computeVertexDistance
                -- @+node:gcross.20100726103932.1711:wrapVertexAround
                ,testGroup "wrapVertexAround"
                    -- @    @+others
                    -- @+node:gcross.20100726103932.1712:examples
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
                    -- @-node:gcross.20100726103932.1712:examples
                    -- @+node:gcross.20100726103932.1713:expected errors
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
                    -- @-node:gcross.20100726103932.1713:expected errors
                    -- @-others
                    ]
                -- @nonl
                -- @-node:gcross.20100726103932.1711:wrapVertexAround
                -- @-others
                ]
        -- @-node:gcross.20100726103932.1706:hexagonal
        -- @+node:gcross.20100726103932.1714:hexagonal (rotated 30 degrees)
        ,testGroup "hexagonal (rotated 30 degrees)" $
            let Periodicity computeVertexDistance wrapVertexAround _ =
                    hexagonalPeriodicityRotatedBy 30 undefined
            in
                -- @        @+others
                -- @+node:gcross.20100726103932.1715:computeVertexDistance
                [testGroup "computeVertexDistance"
                    -- @    @+others
                    -- @+node:gcross.20100726103932.1716:examples
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
                    -- @-node:gcross.20100726103932.1716:examples
                    -- @+node:gcross.20100726103932.1717:bounded by actual distance
                    ,testProperty "bounded by actual distance" $
                        \x y → computeVertexDistance (Vertex x y undefined) <= sqrt (x^2 + y^2)
                    -- @-node:gcross.20100726103932.1717:bounded by actual distance
                    -- @-others
                    ]
                -- @-node:gcross.20100726103932.1715:computeVertexDistance
                -- @-others
                ]
        -- @-node:gcross.20100726103932.1714:hexagonal (rotated 30 degrees)
        -- @-others
        ]
    -- @-node:gcross.20100726103932.1700:Periodicities
    -- @+node:gcross.20100307133316.1312:Tilings
    ,testGroup "Tilings" . const [] $
        [ testGroup tilingName
            .
            catMaybes
            $
            -- @        @+others
            -- @+node:gcross.20100307133316.1313:sum to 360
            [Just . testCase "interior angles sum to 360" $
                    assertEqual
                        "Do the interior angles sum to 360?"
                        360
                        (sum . map polygonInteriorAngle $ tilingPolygons)
            -- @-node:gcross.20100307133316.1313:sum to 360
            -- @+node:gcross.20100308112554.1313:correct steps
            ,fmap (
                testCase "correct steps"
                .
                flip (assertEqual "Are the steps correct?") tilingSteps
             )
             .
             lookup tilingName
             $
                [("quadrille"
                 ,[Step (90 * i) 0 | i ← [0..3]]
                 )
                ,("deltille"
                 ,[Step (60 * i) 0 | i ← [0..5]]
                 )
                ]
            -- @-node:gcross.20100308112554.1313:correct steps
            -- @+node:gcross.20100308112554.1317:invertible steps
            ,Just . testCase "invertible steps" $
                mapM_ (
                    evaluate
                    .
                    findStepNumberForVertex tilingSteps originVertex
                    .
                    stepFromVertex originVertex
                ) tilingSteps
            -- @-node:gcross.20100308112554.1317:invertible steps
            -- @+node:gcross.20100713173607.1583:ordered steps
            ,Just . testCase "ordered steps" $
                assertEqual
                    "Are the steps ordered?"
                    (sortBy (compare `on` stepAngle) tilingSteps)
                    tilingSteps
            -- @-node:gcross.20100713173607.1583:ordered steps
            -- @+node:gcross.20100713173607.1586:zero-based steps
            ,Just . testCase "zero-based steps" $
                assertEqual
                    "Is the first step at angle 0?"
                    0
                .
                stepAngle
                .
                head
                $
                tilingSteps
            -- @-node:gcross.20100713173607.1586:zero-based steps
            -- @+node:gcross.20100726103932.1737:based on periodic lattice
            ,Just . testGroup "based on periodic lattice" $
                [ let lattice@Lattice{..} = generatePeriodicLatticeForTiling tiling radius
                      discrete_lattice@DiscreteLattice{..} = discretizeLattice lattice
                  in testGroup ("radius = " ++ show radius) . catMaybes $
                    -- @        @+others
                    -- @+node:gcross.20100726103932.1738:consistent
                    [Just . testCase "consistent" $ do
                        evaluate latticeVertices
                        evaluate latticeEdges
                        return ()
                    -- @-node:gcross.20100726103932.1738:consistent
                    -- @+node:gcross.20100726103932.1739:correct number of orientations
                    ,Just . testCase "correct number of orientations"
                        .
                        assertEqual
                            "Is the number of computed orientations correct?"
                            tilingNumberOfOrientations
                        .
                        numberOfOrientationsInLattice
                        $
                        lattice
                    -- @-node:gcross.20100726103932.1739:correct number of orientations
                    -- @+node:gcross.20100726103932.1740:correct number of permutations
                    ,Just . testCase "correct number of symmetries"
                        .
                        assertEqual
                            "Is the number of symmetries correct?"
                            tilingNumberOfSymmetries
                        .
                        length
                        .
                        computeSymmetryTransformationsFor tiling radius
                        $
                        lattice
                    -- @-node:gcross.20100726103932.1740:correct number of permutations
                    -- @+node:gcross.20100726103932.1741:valid adjacencies
                    ,Just . testCase "valid adjacencies"
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
                        lattice
                    -- @-node:gcross.20100726103932.1741:valid adjacencies
                    -- @+node:gcross.20100726103932.1742:correct lattice translation symmetry distance
                    ,Just . testCase "correct lattice translation symmetry distance"
                        .
                        assertEqual
                            "Is the distance correct?"
                            (Just tilingTranslationSymmetryDistance)
                        .
                        latticeTranslationDistance
                        $
                        lattice
                    -- @-node:gcross.20100726103932.1742:correct lattice translation symmetry distance
                    -- @+node:gcross.20100726103932.1743:size of lattice invariant under discretization
                    ,Just . testCase "size of lattice invariant under discretization" $ do
                        assertEqual
                            "Has the number of vertices changed?"
                            (Set.size latticeVertices)
                            (Seq.length discreteLatticeVertices)
                        assertEqual
                            "Has the number of edges changed?"
                            (length latticeEdges)
                            (length discreteLatticeEdges)
                    -- @-node:gcross.20100726103932.1743:size of lattice invariant under discretization
                    -- @+node:gcross.20100726103932.1781:correct number of edges
                    ,let correct_edge_counts =
                            [("quadrille",[(1,8),(2,32)])
                            ,("truncated quadrille",[(1,24)])
                            ,("snub quadrille",[(1,20)])
                            ,("hextille",[(1,9)])
                            ,("hexadeltille",[(1,24)])
                            ,("truncated hextille",[(1,36)])
                            ,("deltille",[(1,12)])
                            ,("rhombihexadeltille",[(1,36)])
                            ,("isosnub quadrille",[(1,30)])
                            ]
                     in lookup tilingName correct_edge_counts
                        >>=
                        lookup radius
                        >>=
                        return
                        .
                        testCase "correct number of edges"
                        .
                        flip (assertEqual "Is the number of edges correct?")
                            (length latticeEdges)
                    -- @-node:gcross.20100726103932.1781:correct number of edges
                    -- @+node:gcross.20100726103932.1770:correct pictures
                    ,let correct_pictures =
                            -- @        @+others
                            -- @+node:gcross.20100726103932.1771:quadrille
                            [("quadrille",
                              [(1,["00"
                                  ,"00"
                                  ]
                               )
                              ,(2,["0000"
                                  ,"0000"
                                  ,"0000"
                                  ,"0000"
                                  ])
                              ]
                             )
                            -- @-node:gcross.20100726103932.1771:quadrille
                            -- @+node:gcross.20100726103932.1772:truncated quadrille
                            ,("truncated quadrille",
                              [(1,["  03  "
                                  ,"  12  "
                                  ,"03  03"
                                  ,"12  12"
                                  ,"  03  "
                                  ,"  12  "
                                  ])
                              ]
                             )
                            -- @-node:gcross.20100726103932.1772:truncated quadrille
                            -- @+node:gcross.20100726103932.1773:snub quadrille
                            ,("snub quadrille",
                              [(1,["  1  "
                                  ," 3 2 "
                                  ,"2   0"
                                  ," 0 1 "
                                  ,"  3  "
                                  ])
                              ]
                             )
                            -- @-node:gcross.20100726103932.1773:snub quadrille
                            -- @+node:gcross.20100726103932.1774:hextille
                            ,("hextille",
                              [(1,[" 01 "
                                  ,"1  0"
                                  ," 01 "
                                  ])
                              ]
                             )
                            -- @-node:gcross.20100726103932.1774:hextille
                            -- @+node:gcross.20100726103932.1775:hexadeltille
                            ,("hexadeltille",
                              [(1,["   1   "
                                  ,"0 2 0 2"
                                  ," 1   1 "
                                  ,"2 0 2 0"
                                  ,"   1   "
                                  ])
                              ,(2,["       2        "
                                  ,"      1   1     "
                                  ," 2 0 2 0 2 0 2  "
                                  ,"1   1   1   1   "
                                  ," 0 2 0 2 0 2 0 2"
                                  ,"  1   1   1   1 "
                                  ," 2 0 2 0 2 0 2 0"
                                  ,"1   1   1   1   "
                                  ," 0 2 0 2 0 2 0  "
                                  ,"      1   1     "
                                  ,"       0        "
                                  ])
                              ]

                             )
                            -- @-node:gcross.20100726103932.1775:hexadeltille
                            -- @+node:gcross.20100726103932.1776:truncated hextille
                            ,("truncated hextille",
                              [(1,["     2     "
                                  ,"     5     "
                                  ,"3   1 3   1"
                                  ," 0 4   0 4 "
                                  ,"  2     2  "
                                  ,"  5     5  "
                                  ," 1 3   1 3 "
                                  ,"4   0 4   0"
                                  ,"     2     "
                                  ,"     5     "
                                  ])
                              ]
                             )
                            -- @-node:gcross.20100726103932.1776:truncated hextille
                            -- @+node:gcross.20100726103932.1777:deltille
                            ,("deltille",
                              [(1,[" 0 "
                                  ,"0 0"
                                  ," 0 "
                                  ])
                              ,(2,["  0 0   " 
                                  ," 0 0 0 0"
                                  ,"0 0 0 0 "
                                  ," 0 0 0 0"
                                  ,"  0 0   "
                                  ])
                              ]
                             )
                            -- @-node:gcross.20100726103932.1777:deltille
                            -- @+node:gcross.20100726103932.1778:rhombihexadeltille
                            ,("rhombihexadeltille",
                              [(1,["   5 3   "
                                  ," 4     4 "
                                  ,"    1    "
                                  ,"0 2   0 2"
                                  ,"5 3   5 3"
                                  ,"    4    "
                                  ," 1     1 "
                                  ,"   0 2   "
                                  ])
                              ]
                             )
                            -- @-node:gcross.20100726103932.1778:rhombihexadeltille
                            -- @+node:gcross.20100726103932.1779:isosnub quadrille
                            ,("isosnub quadrille",
                              [(1,[" 0 0 0"
                                  ,"1 1 1 "
                                  ,"0 0 0 "
                                  ," 1 1 1"
                                  ])
                              ,(2,[" 0 0 0 0 0 0"
                                  ,"1 1 1 1 1 1 "
                                  ,"0 0 0 0 0 0 "
                                  ," 1 1 1 1 1 1"
                                  ," 0 0 0 0 0 0"
                                  ,"1 1 1 1 1 1 "
                                  ,"0 0 0 0 0 0 "
                                  ," 1 1 1 1 1 1"
                                  ])
                              ]
                             )
                            -- @-node:gcross.20100726103932.1779:isosnub quadrille
                            -- @-others
                            ]
                     in lookup tilingName correct_pictures
                        >>=
                        lookup radius
                        >>=
                        return
                        .
                        testCase "correct picture"
                        .
                        flip (assertEqual "Is the picture correct?")
                            (drawDiscreteLattice discrete_lattice)
                        .
                        unlines
                    -- @-node:gcross.20100726103932.1770:correct pictures
                    -- @+node:gcross.20100726103932.1791:expected symmetries
                    ,fmap (
                        testGroup "expected symmetries"
                        .
                        map (\(description,f) →
                            testCase description
                            .
                            assertEqual
                                ("Did drawing when applying the " ++ description ++ " transformation?")
                                (Just . drawDiscreteLattice $ discrete_lattice)
                            .
                            fmap (drawVertices . snd)
                            .
                            applySymmetryTransformationToVertices
                                tiling
                                radius
                                latticeVertices
                            $
                            f
                        )
                     )
                     .
                     lookup tilingName
                     $
                     let r angle = (show (floor angle) ++ "-degree counter-clockwise rotation",(+angle))
                     in  [("quadrille",map r [90,180,270])
                         ,("truncated quadrille",map r [90,180,270])
                         ,("hexadeltille",map r [60,120..300])
                         ]
                    -- @-node:gcross.20100726103932.1791:expected symmetries
                    -- @+node:gcross.20100726103932.1755:symmetries preserve code
                    ,if radius > 1 then Nothing else Just . testProperty "symmetries preserve code" $
                        arbitraryLabelingForLattice lattice
                        >>=
                        \labeling → return $
                            let (first_solution:rest_solutions) =
                                    map (
                                        solveForLabeling (latticeToScanConfiguration discrete_lattice)
                                        .
                                        permuteLatticeLabeling labeling
                                    ) tilingSymmetries
                            in all (== first_solution) rest_solutions
                    -- @nonl
                    -- @-node:gcross.20100726103932.1755:symmetries preserve code
                    -- @-others
                    ]
                | radius ← [1,2,3,4,5,6]
                , tilingName /= "deltille" || radius `mod` 3 > 0
                ]
            -- @-node:gcross.20100726103932.1737:based on periodic lattice
            -- @+node:gcross.20100312175547.1383:iterable 8 times
            ,Just . testCase "iterable 8 times" $
                let lattices =
                        sel1
                        .
                        fst
                        .
                        runLatticeMonadForTiling tilingName
                        .
                        iteratePrunedLattices
                            (liftA2 (max `on` abs) vertexLocationX vertexLocationY)
                            (+2)
                            2
                            [originVertex]
                        $
                        8
                in do
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
            -- @-node:gcross.20100312175547.1383:iterable 8 times
            -- @-others
            ]
        | tiling@Tiling{..} ← tilings
        ]
    -- @-node:gcross.20100307133316.1312:Tilings
    -- @-others
    -- @-node:gcross.20100302201317.1388:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100302201317.1270:@thin test.hs
-- @-leo

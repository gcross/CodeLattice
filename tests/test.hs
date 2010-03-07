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
import Control.Monad

import Debug.Trace

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import CodeLattice
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
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100302201317.1388:<< Tests >>
    -- @+others
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
    -- @-others
    -- @-node:gcross.20100302201317.1388:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100302201317.1270:@thin test.hs
-- @-leo

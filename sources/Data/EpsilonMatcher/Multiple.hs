-- @+leo-ver=4-thin
-- @+node:gcross.20100714141137.2412:@thin EpsilonMatcher/Multiple.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100714141137.2528:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100714141137.2528:<< Language extensions >>
-- @nl

module Data.EpsilonMatcher.Multiple where

-- @<< Import needed modules >>
-- @+node:gcross.20100714141137.2413:<< Import needed modules >>
import Control.Arrow
import Control.Monad.Trans.State.Strict

import Data.EpsilonMatcher
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
-- @-node:gcross.20100714141137.2413:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100714141137.2414:Types
-- @+node:gcross.20100714141137.2415:MultipleEpsilonMatcherState
type MultipleEpsilonMatcherState valueType resultType = State (IntMap (EpsilonMatcher valueType)) resultType
-- @-node:gcross.20100714141137.2415:MultipleEpsilonMatcherState
-- @-node:gcross.20100714141137.2414:Types
-- @+node:gcross.20100714141137.2416:Functions
-- @+node:gcross.20100714141137.2417:Pure
-- @+node:gcross.20100714141137.2418:matchIn
matchIn ::
    (Ord valueType, Num valueType) =>
    Int →
    valueType →
    IntMap (EpsilonMatcher valueType) →
    (Int,IntMap (EpsilonMatcher valueType))
matchIn matcher_index lookup_value matchers =
    let old_matcher = fromJust . IntMap.lookup matcher_index $ matchers
        (match_key,new_matcher) = match lookup_value old_matcher
    in (match_key,IntMap.insert matcher_index new_matcher matchers)
-- @nonl
-- @-node:gcross.20100714141137.2418:matchIn
-- @+node:gcross.20100714141137.2530:allMatchValuesIn
allMatchValuesIn :: Int → IntMap (EpsilonMatcher valueType) → [valueType]
allMatchValuesIn matcher_index = allMatchValues . fromJust . IntMap.lookup matcher_index
-- @-node:gcross.20100714141137.2530:allMatchValuesIn
-- @+node:gcross.20100714141137.2548:reverseMatchMapIn
reverseMatchMapIn :: Int → IntMap (EpsilonMatcher valueType) → IntMap valueType
reverseMatchMapIn matcher_index = reverseMatchMap . fromJust . IntMap.lookup matcher_index
-- @-node:gcross.20100714141137.2548:reverseMatchMapIn
-- @-node:gcross.20100714141137.2417:Pure
-- @+node:gcross.20100714141137.2419:Monadic
-- @+node:gcross.20100714141137.2420:getMatchMaps
getMatchMaps :: MultipleEpsilonMatcherState valueType [MatchMap]
getMatchMaps = gets (map computeMatchMap . IntMap.elems)
-- @-node:gcross.20100714141137.2420:getMatchMaps
-- @+node:gcross.20100714141137.2531:getAllMatchValuesIn
getAllMatchValuesIn :: Int → MultipleEpsilonMatcherState valueType [valueType]
getAllMatchValuesIn = gets . allMatchValuesIn

-- @-node:gcross.20100714141137.2531:getAllMatchValuesIn
-- @+node:gcross.20100714141137.2550:getReverseMatchMapIn
getReverseMatchMapIn :: Int → MultipleEpsilonMatcherState valueType (IntMap valueType)
getReverseMatchMapIn = gets . reverseMatchMapIn
-- @-node:gcross.20100714141137.2550:getReverseMatchMapIn
-- @+node:gcross.20100714141137.2421:lookupMatchIn
lookupMatchIn ::
    (Ord valueType, Num valueType) =>
    Int →
    valueType →
    MultipleEpsilonMatcherState valueType Int
lookupMatchIn matcher_index = state . matchIn matcher_index
-- @nonl
-- @-node:gcross.20100714141137.2421:lookupMatchIn
-- @+node:gcross.20100714141137.2422:runEpsilonMatcher
runMultipleEpsilonMatchers ::
    [valueType] →
    MultipleEpsilonMatcherState valueType resultType →
    (resultType,[MatchMap])
runMultipleEpsilonMatchers tolerances stateRunner =
    second (map computeMatchMap . IntMap.elems)
    .
    runState stateRunner
    .
    IntMap.fromAscList
    .
    zip [0..]
    .
    map newEpsilonMatcher
    $
    tolerances
-- @nonl
-- @-node:gcross.20100714141137.2422:runEpsilonMatcher
-- @-node:gcross.20100714141137.2419:Monadic
-- @-node:gcross.20100714141137.2416:Functions
-- @-others
-- @-node:gcross.20100714141137.2412:@thin EpsilonMatcher/Multiple.hs
-- @-leo

-- @+leo-ver=4-thin
-- @+node:gcross.20100714141137.2394:@thin EpsilonMatcher.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100714141137.2529:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100714141137.2529:<< Language extensions >>
-- @nl

module Data.EpsilonMatcher where

-- @<< Import needed modules >>
-- @+node:gcross.20100714141137.2395:<< Import needed modules >>
import Control.Arrow
import Control.Monad.Trans.State.Strict

import qualified Data.COrdering as COrdering
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Tree.AVL (AVL)
import qualified Data.Tree.AVL as AVL
-- @-node:gcross.20100714141137.2395:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100714141137.2396:Types
-- @+node:gcross.20100714141137.2397:Match
data Match valueType = Match
    {   matchValue :: valueType
    ,   matchKey :: Int
    }
-- @-node:gcross.20100714141137.2397:Match
-- @+node:gcross.20100714141137.2398:MatchMap
type MatchMap = IntMap Int
-- @nonl
-- @-node:gcross.20100714141137.2398:MatchMap
-- @+node:gcross.20100714141137.2399:EpsilonMatcher
data EpsilonMatcher valueType = EpsilonMatcher
    {   epsilonMatcherTree :: AVL (Match valueType)
    ,   epsilonMatcherNextIndex :: Int
    ,   epsilonMatcherTolerance :: valueType
    }
-- @-node:gcross.20100714141137.2399:EpsilonMatcher
-- @+node:gcross.20100714141137.2400:EpsilonMatcherState
type EpsilonMatcherState valueType resultType = State (EpsilonMatcher valueType) resultType
-- @-node:gcross.20100714141137.2400:EpsilonMatcherState
-- @-node:gcross.20100714141137.2396:Types
-- @+node:gcross.20100714141137.2401:Functions
-- @+node:gcross.20100714141137.2402:Pure
-- @+node:gcross.20100714141137.2403:newEpsilonMatcher
newEpsilonMatcher :: valueType → EpsilonMatcher valueType
newEpsilonMatcher tolerance = EpsilonMatcher AVL.empty 0 tolerance
-- @nonl
-- @-node:gcross.20100714141137.2403:newEpsilonMatcher
-- @+node:gcross.20100714141137.2404:computeMatchMap
computeMatchMap :: EpsilonMatcher valueType → MatchMap
computeMatchMap =
    IntMap.fromList
    .
    flip zip [0..]
    .
    map matchKey
    .
    AVL.asListL
    .
    epsilonMatcherTree
-- @nonl
-- @-node:gcross.20100714141137.2404:computeMatchMap
-- @+node:gcross.20100714141137.2405:match
match ::
    (Ord valueType, Num valueType) =>
    valueType →
    EpsilonMatcher valueType →
    (Int,EpsilonMatcher valueType)
match lookup_value matcher@(EpsilonMatcher match_tree next_index tolerance) =
    case AVL.tryRead match_tree comparer of
        Just match_key → (match_key,matcher)
        Nothing →
            (next_index
            ,EpsilonMatcher
                (AVL.push comparer2 (Match lookup_value next_index) match_tree)
                (next_index+1)
                tolerance
            )
  where
    comparer match@(Match match_value match_key) =
        if abs (lookup_value-match_value) <= tolerance
            then COrdering.Eq match_key
            else comparer2 match
    comparer2 (Match match_value _) =
        if lookup_value < match_value
            then COrdering.Lt
            else COrdering.Gt
-- @nonl
-- @-node:gcross.20100714141137.2405:match
-- @+node:gcross.20100714141137.2545:reverseMatchMap
reverseMatchMap :: EpsilonMatcher valueType → IntMap valueType
reverseMatchMap =
    IntMap.fromList
    .
    map (matchKey &&& matchValue)
    .
    AVL.asListL
    .
    epsilonMatcherTree
-- @-node:gcross.20100714141137.2545:reverseMatchMap
-- @+node:gcross.20100714141137.2406:allMatchValues
allMatchValues :: EpsilonMatcher valueType → [valueType]
allMatchValues =
    map matchValue
    .
    AVL.asListL
    .
    epsilonMatcherTree
-- @nonl
-- @-node:gcross.20100714141137.2406:allMatchValues
-- @-node:gcross.20100714141137.2402:Pure
-- @+node:gcross.20100714141137.2407:Monadic
-- @+node:gcross.20100714141137.2408:getMatchMap
getMatchMap :: EpsilonMatcherState valueType MatchMap
getMatchMap = gets computeMatchMap
-- @-node:gcross.20100714141137.2408:getMatchMap
-- @+node:gcross.20100714141137.2409:getAllMatchValues
getAllMatchValues :: EpsilonMatcherState valueType [valueType]
getAllMatchValues = gets allMatchValues
-- @-node:gcross.20100714141137.2409:getAllMatchValues
-- @+node:gcross.20100714141137.2546:getReverseMatchMap
getReverseMatchMap :: EpsilonMatcherState valueType (IntMap valueType)
getReverseMatchMap = gets reverseMatchMap
-- @-node:gcross.20100714141137.2546:getReverseMatchMap
-- @+node:gcross.20100714141137.2410:lookupMatch
lookupMatch ::
    (Ord valueType, Num valueType) =>
    valueType →
    EpsilonMatcherState valueType Int
lookupMatch = state . match
-- @nonl
-- @-node:gcross.20100714141137.2410:lookupMatch
-- @+node:gcross.20100714141137.2411:runEpsilonMatcher
runEpsilonMatcher ::
    valueType →
    EpsilonMatcherState valueType resultType →
    (resultType, MatchMap)
runEpsilonMatcher tolerance stateRunner =
    second computeMatchMap
    .
    runState stateRunner
    .
    newEpsilonMatcher
    $
    tolerance
-- @nonl
-- @-node:gcross.20100714141137.2411:runEpsilonMatcher
-- @-node:gcross.20100714141137.2407:Monadic
-- @-node:gcross.20100714141137.2401:Functions
-- @-others
-- @-node:gcross.20100714141137.2394:@thin EpsilonMatcher.hs
-- @-leo

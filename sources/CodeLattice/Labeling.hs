-- @+leo-ver=4-thin
-- @+node:gcross.20100723201654.1698:@thin Labeling.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100723201654.1699:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100723201654.1699:<< Language extensions >>
-- @nl

module CodeLattice.Labeling where

-- @<< Import needed modules >>
-- @+node:gcross.20100723201654.1721:<< Import needed modules >>
import Control.Monad
import Data.List

import CodeLattice
-- @nonl
-- @-node:gcross.20100723201654.1721:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100723201654.1714:Types
-- @+node:gcross.20100723201654.1716:VertexLabelingPermutation
newtype VertexLabelingPermutation = VertexLabelingPermutation
    { unwrapVertexLabelingPermutation :: [Int]
    } deriving (Eq)
-- @-node:gcross.20100723201654.1716:VertexLabelingPermutation
-- @+node:gcross.20100723201654.1718:LatticeLabelingPermutation
newtype LatticeLabelingPermutation = LatticeLabelingPermutation
    { unwrapLatticeLabelingPermutation :: [(Int,VertexLabelingPermutation)]
    } deriving (Eq)
-- @-node:gcross.20100723201654.1718:LatticeLabelingPermutation
-- @+node:gcross.20100723201654.1720:VertexClass(es)
newtype VertexClass = VertexClass { unwrapVertexClass :: [ApproximateDouble] }
newtype VertexClasses = VertexClasses { unwrapVertexClasses :: [VertexClass] }
-- @-node:gcross.20100723201654.1720:VertexClass(es)
-- @-node:gcross.20100723201654.1714:Types
-- @+node:gcross.20100723201654.1706:Functions
-- @+node:gcross.20100723201654.1707:(?→?)
(?→?) :: VertexClass → VertexClass → Maybe VertexLabelingPermutation
(?→?) (VertexClass angles) =
    fmap VertexLabelingPermutation
    .
    sequence
    .
    map (flip elemIndex angles)
    .
    unwrapVertexClass
-- @-node:gcross.20100723201654.1707:(?→?)
-- @+node:gcross.20100723201654.1708:(??→?)
(??→?) :: VertexClasses → VertexClass → Maybe (Int,VertexLabelingPermutation)
(VertexClasses vcs) ??→? vc2 =
    msum
    .
    zipWith (\index vc1 → fmap (index,) (vc1 ?→? vc2))
        [0..]
    $
    vcs
-- @-node:gcross.20100723201654.1708:(??→?)
-- @+node:gcross.20100723201654.1709:(??→??)
(??→??) :: VertexClasses → VertexClasses → Maybe LatticeLabelingPermutation
(??→??) vertex_classes =
    fmap LatticeLabelingPermutation
    .
    sequence
    .
    map (vertex_classes ??→?)
    .
    unwrapVertexClasses
-- @-node:gcross.20100723201654.1709:(??→??)
-- @+node:gcross.20100723201654.1710:(|⇆)
(|⇆) :: ApproximateDouble → ApproximateDouble → ApproximateDouble
reflection_axis_angle |⇆ angle = 2*reflection_axis_angle - angle
-- @-node:gcross.20100723201654.1710:(|⇆)
-- @-node:gcross.20100723201654.1706:Functions
-- @-others
-- @-node:gcross.20100723201654.1698:@thin Labeling.hs
-- @-leo

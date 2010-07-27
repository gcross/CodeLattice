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
import Data.Maybe

import CodeLattice
-- @nonl
-- @-node:gcross.20100723201654.1721:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100723201654.1714:Types
-- @+node:gcross.20100727110906.1649:VertexLabeling
newtype VertexLabeling = VertexLabeling { unwrapVertexLabeling :: [Int] } deriving (Eq,Ord,Show)


-- @-node:gcross.20100727110906.1649:VertexLabeling
-- @+node:gcross.20100727110906.1651:LatticeLabeling
newtype LatticeLabeling = LatticeLabeling { unwrapLatticeLabeling :: [VertexLabeling] } deriving (Eq,Ord,Show)
-- @nonl
-- @-node:gcross.20100727110906.1651:LatticeLabeling
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
-- @+node:gcross.20100727110906.1667:canonicalizeLatticeLabeling
canonicalizeLatticeLabeling :: LatticeLabeling → LatticeLabeling
canonicalizeLatticeLabeling = LatticeLabeling . map canonicalizeVertexLabeling . unwrapLatticeLabeling
-- @nonl
-- @-node:gcross.20100727110906.1667:canonicalizeLatticeLabeling
-- @+node:gcross.20100727110906.1664:canonicalizeVertexLabeling
canonicalizeVertexLabeling :: VertexLabeling → VertexLabeling
canonicalizeVertexLabeling (VertexLabeling old_labeling) =
    VertexLabeling
    .
    map ((+1) . fromJust . flip elemIndex (nub old_labeling))
    $
    old_labeling
-- @-node:gcross.20100727110906.1664:canonicalizeVertexLabeling
-- @+node:gcross.20100727110906.1653:flattenLatticeLabeling
flattenLatticeLabeling :: LatticeLabeling → [Int]
flattenLatticeLabeling =
    concat
    .
    transpose
    .
    map unwrapVertexLabeling
    .
    unwrapLatticeLabeling
-- @-node:gcross.20100727110906.1653:flattenLatticeLabeling
-- @+node:gcross.20100727110906.1666:generateLatticeLabelings
generateLatticeLabelings :: Int → Int → [LatticeLabeling]
generateLatticeLabelings number_of_vertices number_of_rays =
    map LatticeLabeling
    .
    replicateM number_of_vertices
    .
    generateVertexLabelings
    $
    number_of_rays
-- @nonl
-- @-node:gcross.20100727110906.1666:generateLatticeLabelings
-- @+node:gcross.20100727110906.1665:generateVertexLabelings
generateVertexLabelings :: Int → [VertexLabeling]
generateVertexLabelings = map (VertexLabeling . (1:)) . go . (\x → x-1)
  where
    go 0 = [[]]
    go n =
        (map (1:) (go (n-1)))
        ++
        (map (2:) (replicateM (n-1) [1..3]))
-- @-node:gcross.20100727110906.1665:generateVertexLabelings
-- @+node:gcross.20100727110906.1669:permuteLatticeLabeling
permuteLatticeLabeling ::  LatticeLabeling → LatticeLabelingPermutation → LatticeLabeling
permuteLatticeLabeling (LatticeLabeling old_labeling) =
    LatticeLabeling
    .
    map (uncurry (permuteVertexLabeling . (old_labeling !!)))
    .
    unwrapLatticeLabelingPermutation
-- @-node:gcross.20100727110906.1669:permuteLatticeLabeling
-- @+node:gcross.20100727110906.1668:permuteVertexLabeling
permuteVertexLabeling ::  VertexLabeling → VertexLabelingPermutation → VertexLabeling
permuteVertexLabeling (VertexLabeling old_labeling) =
    VertexLabeling
    .
    map (old_labeling !!)
    .
    unwrapVertexLabelingPermutation
-- @-node:gcross.20100727110906.1668:permuteVertexLabeling
-- @-node:gcross.20100723201654.1706:Functions
-- @-others
-- @-node:gcross.20100723201654.1698:@thin Labeling.hs
-- @-leo

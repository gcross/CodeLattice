-- @+leo-ver=4-thin
-- @+node:gcross.20100802171839.1669:@thin show-latex-combinatorics-table.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100802171839.1670:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100802171839.1670:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100802171839.1671:<< Import needed modules >>
import Control.Monad
import Text.Printf

import CodeLattice.Tilings
-- @-node:gcross.20100802171839.1671:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100802171839.1672:main
main = forM_ tilings $ \(Tiling{..}) →
    printf "%s & %i & %i & %i\\\\\n"
        tilingName
        tilingNumberOfOrientations
        tilingNumberOfRays
        tilingNumberOfLabelings
-- @-node:gcross.20100802171839.1672:main
-- @-others
-- @-node:gcross.20100802171839.1669:@thin show-latex-combinatorics-table.hs
-- @-leo

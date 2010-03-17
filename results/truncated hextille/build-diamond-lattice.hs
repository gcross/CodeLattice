-- @+leo-ver=4-thin
-- @+node:gcross.20100316190653.1534:@thin build-diamond-lattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100316190653.1535:<< Language extensions >>
-- @-node:gcross.20100316190653.1535:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100316190653.1536:<< Import needed modules >>
import System.Environment

import TruncatedHextille
-- @-node:gcross.20100316190653.1536:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100316190653.1537:main
main =
    fmap (map read) getArgs
    >>=
    \[size] ->
        putEdges
        .
        concat
        .
        take size
        .
        iterate (\edges -> concat . map ($ edges) . map shift $ [L,R,U,D,UL,UR,DL,DR])
        $
        edges
-- @-node:gcross.20100316190653.1537:main
-- @-others
-- @-node:gcross.20100316190653.1534:@thin build-diamond-lattice.hs
-- @-leo

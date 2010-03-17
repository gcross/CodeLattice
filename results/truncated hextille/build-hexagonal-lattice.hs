-- @+leo-ver=4-thin
-- @+node:gcross.20100316190653.1526:@thin build-hexagonal-lattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100316190653.1527:<< Language extensions >>
-- @-node:gcross.20100316190653.1527:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100316190653.1528:<< Import needed modules >>
import System.Environment

import TruncatedHextille
-- @-node:gcross.20100316190653.1528:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100316190653.1529:main
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
        iterate (\edges -> concat . map ($ edges) . map shift $ [L,R,UL,UR,DL,DR])
        $
        edges

-- @-node:gcross.20100316190653.1529:main
-- @-others
-- @-node:gcross.20100316190653.1526:@thin build-hexagonal-lattice.hs
-- @-leo

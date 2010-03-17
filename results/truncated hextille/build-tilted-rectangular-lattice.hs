-- @+leo-ver=4-thin
-- @+node:gcross.20100316190653.1508:@thin build-tilted-rectangular-lattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100316190653.1509:<< Language extensions >>
-- @-node:gcross.20100316190653.1509:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100316190653.1510:<< Import needed modules >>
import System.Environment

import TruncatedHextille
-- @-node:gcross.20100316190653.1510:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100316190653.1521:main
main =
    fmap (map read) getArgs
    >>=
    \[width,height] ->
        putEdges
        .
        concat
        .
        take height
        .
        iterate (shift UR)
        .
        concat
        .
        take width
        .
        iterate (shift R)
        $
        edges

-- @-node:gcross.20100316190653.1521:main
-- @-others
-- @-node:gcross.20100316190653.1508:@thin build-tilted-rectangular-lattice.hs
-- @-leo

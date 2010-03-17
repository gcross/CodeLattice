-- @+leo-ver=4-thin
-- @+node:gcross.20100316190653.1551:@thin build-uneven-rectangular-lattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100316190653.1552:<< Language extensions >>
-- @-node:gcross.20100316190653.1552:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100316190653.1553:<< Import needed modules >>
import Data.List

import System.Environment

import TruncatedHextille
-- @-node:gcross.20100316190653.1553:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100316190653.1558:main
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
        concat
        .
        transpose
        .
        map ($ edges)
        $
        [iterate (shift U) . concat . take (width-1) . iterate (shift R)
        ,iterate (shift U) . shift UL . concat . take width . iterate (shift R)
        ]        
-- @-node:gcross.20100316190653.1558:main
-- @-others
-- @-node:gcross.20100316190653.1551:@thin build-uneven-rectangular-lattice.hs
-- @-leo

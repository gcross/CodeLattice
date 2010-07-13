-- @+leo-ver=4-thin
-- @+node:gcross.20100316190653.1542:@thin build-rectangular-lattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100316190653.1543:<< Language extensions >>
-- @-node:gcross.20100316190653.1543:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100316190653.1544:<< Import needed modules >>
import System.Environment

import TruncatedHextille
-- @-node:gcross.20100316190653.1544:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100316190653.1556:main
main =
    fmap (map read) getArgs
    >>=
    \[width,height] →
        putEdges
        .
        concat
        .
        take height
        .
        goUp
        .
        concat
        .
        take width
        .
        iterate (shift R)
        $
        edges
  where
    goUp :: [Edge] → [[Edge]]
    goUp edges = edges : first_shift : goUp second_shift
      where
        first_shift = shift UL edges
        second_shift = shift UR first_shift
-- @nonl
-- @-node:gcross.20100316190653.1556:main
-- @-others
-- @-node:gcross.20100316190653.1542:@thin build-rectangular-lattice.hs
-- @-leo

-- @+leo-ver=4-thin
-- @+node:gcross.20100727222803.1691:@thin count-labelings.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100727222803.1693:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100727222803.1693:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100727222803.1694:<< Import needed modules >>
import Data.List

import System.Environment
import System.Exit

import Text.Printf

import CodeLattice.Tilings
-- @-node:gcross.20100727222803.1694:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100727222803.1697:Functions
-- @+node:gcross.20100727222803.1698:getArguments
getArguments :: IO Tiling
getArguments =
    getArgs
    >>=
    \args →
        case args of
            [x] → return (x)
            _ → do
                putStrLn "Usage:  count-labelings <tiling>"
                exitFailure
    >>=
    \tiling_name →
        case find ((== tiling_name) . tilingName) tilings of
            Just tiling → return tiling
            _ → do
                putStrLn "Tiling must be one of the following:"
                mapM_ (putStrLn . ('\t':) . tilingName) tilings -- '
                exitFailure
-- @-node:gcross.20100727222803.1698:getArguments
-- @-node:gcross.20100727222803.1697:Functions
-- @-others

main =
    getArguments
    >>=
    putStrLn
        .
        show
        .
        tilingNumberOfLabelings
-- @-node:gcross.20100727222803.1691:@thin count-labelings.hs
-- @-leo

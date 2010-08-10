-- @+leo-ver=4-thin
-- @+node:gcross.20100728132013.1636:@thin count-physical-qubits.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100728132013.2005:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100728132013.2005:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100728132013.2006:<< Import needed modules >>
import Prelude hiding (catch)

import Control.Exception
import Control.Monad

import Data.List
import qualified Data.Set as Set

import System.Environment
import System.Exit

import Text.Printf
import Text.Read

import CodeLattice
import CodeLattice.Tilings
-- @-node:gcross.20100728132013.2006:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100728132013.2007:Functions
-- @+node:gcross.20100728132013.2008:getArguments
getArguments :: IO (Tiling,Int)
getArguments = do
    args ← getArgs
    (tiling_name,radius_as_string) ←
        case args of
            [x,y] → return (x,y)
            _ → do
                putStrLn "Usage:  count-physical-qubits <tiling> <radius>"
                exitFailure
    tiling ←
        case find ((== tiling_name) . tilingName) tilings of
            Just tiling → return tiling
            _ → do
                putStrLn "Tiling must be one of the following:"
                mapM_ (putStrLn . ('\t':) . tilingName) tilings -- '
                exitFailure
    radius ←
        case reads radius_as_string of
            ((radius,_):_)
              | radius > 0 → return radius
              | otherwise → do
                    putStrLn "The radius must be greater than zero."
                    exitFailure
            _ → do
                putStrLn "The radius must be an integer."
                exitFailure
    return (tiling,radius)
-- @-node:gcross.20100728132013.2008:getArguments
-- @-node:gcross.20100728132013.2007:Functions
-- @+node:gcross.20100810123019.1697:main
main =
    getArguments
    >>=
    putStrLn
        .
        show
        .
        uncurry computeNumberOfQubitsInPeriodicLatticeForTiling
-- @-node:gcross.20100810123019.1697:main
-- @-others
-- @-node:gcross.20100728132013.1636:@thin count-physical-qubits.hs
-- @-leo

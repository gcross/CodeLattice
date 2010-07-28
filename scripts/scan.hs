-- @+leo-ver=4-thin
-- @+node:gcross.20100727222803.1685:@thin scan.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100727222803.1686:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100727222803.1686:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100727222803.1687:<< Import needed modules >>
import Prelude hiding (catch)

import Control.Exception
import Control.Monad

import Data.List

import System.Environment
import System.Exit

import Text.Printf
import Text.Read

import CodeLattice.Discrete
import CodeLattice.Labeling
import CodeLattice.Scanning
import CodeLattice.Tilings
-- @-node:gcross.20100727222803.1687:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100727222803.1688:Functions
-- @+node:gcross.20100727222803.1689:getArguments
getArguments :: IO (Tiling,Int)
getArguments = do
    args ← getArgs
    (tiling_name,radius_as_string) ←
        case args of
            [x,y] → return (x,y)
            _ → do
                putStrLn "Usage:  scan <tiling> <radius>"
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
-- @-node:gcross.20100727222803.1689:getArguments
-- @-node:gcross.20100727222803.1688:Functions
-- @-others

main = do
    (tiling@Tiling{..},radius) ← getArguments
    let solveFor =
            solveForLabelingWithVerbosity False
            .
            latticeToScanConfiguration
            .
            discretizeLattice
            $
            generatePeriodicLatticeForTiling tiling radius
        labelings = generateLatticeLabelingsForTiling tiling
    forM_ (zip [(0::Int)..] labelings) $ \(n,labeling) →
        let minimal_labeling =
                minimum
                .
                map (permuteLatticeLabeling labeling)
                $
                tilingSymmetries
        in if minimal_labeling < labeling then return () else do
            solution <- solveFor labeling
            case solutionLogicalQubitDistances solution of
                [] → return ()
                distances@(d:_) → if d <= 2 then return () else putStrLn $
                    show n
                    ++ " -> " ++
                    show distances
-- @-node:gcross.20100727222803.1685:@thin scan.hs
-- @-leo

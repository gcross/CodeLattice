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
import Control.Monad.Trans

import Data.List

import Database.Enumerator

import System.Environment
import System.Exit

import Text.Printf
import Text.Read

import CodeLattice.Database
import CodeLattice.Discrete
import CodeLattice.Labeling
import CodeLattice.Scanning
import CodeLattice.Tilings
-- @-node:gcross.20100727222803.1687:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100727222803.1688:Functions
-- @+node:gcross.20100727222803.1689:getArguments
getArguments :: IO (Tiling,Int,Integer,Integer)
getArguments = do
    args ← getArgs
    (tiling_name,radius_as_string,maybe_skip_as_string,maybe_offset_as_string) ←
        case args of
            [x,y] → return (x,y,Nothing,Nothing)
            [x,y,z,w] → return (x,y,Just z,Just w)
            _ → do
                putStrLn "Usage:  scan <tiling> <radius> [<skip> <offset>]"
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
              | radius <= 0 → do
                    putStrLn "The radius must be greater than zero."
                    exitFailure
              | otherwise → return radius
            _ → do
                putStrLn "The radius must be an integer."
                exitFailure
    skip ←
        case fmap reads maybe_skip_as_string of
            Nothing → return 1
            Just ((skip,_):_)
              | skip <= 0 → do
                    putStrLn "The skip must be greater than zero."
                    exitFailure
              | otherwise → return skip
            _ → do
                putStrLn "The skip must be an integer."
                exitFailure
    offset ←
        case fmap reads maybe_offset_as_string of
            Nothing → return 1
            Just ((offset,_):_)
              | offset <= 0 → do
                    putStrLn "The offset must be greater than zero."
                    exitFailure
              | offset >= skip → do
                    putStrLn "The offset must be less than the skip."
                    exitFailure
              | otherwise → return skip
            _ → do
                putStrLn "The offset must be an integer."
                exitFailure
    return (tiling,radius,skip,offset)
-- @-node:gcross.20100727222803.1689:getArguments
-- @+node:gcross.20100728191908.1636:skipEvery
skipEvery :: Int → [a] → [a]
skipEvery n = unfoldr go
  where
    go [] = Nothing
    go (x:xs) = Just (x,drop (n-1) xs)
-- @-node:gcross.20100728191908.1636:skipEvery
-- @-node:gcross.20100727222803.1688:Functions
-- @-others

main = do
    (tiling@Tiling{..},radius,skip,offset) ← getArguments
    let solveFor =
            solveForLabelingWithVerbosity False
            .
            latticeToScanConfiguration
            .
            discretizeLattice
            $
            generatePeriodicLatticeForTiling tiling radius
        start = 1
    connection ← makeConnection "scanner"
    withSession connection $ do
        has_been_scanned ← checkIfScanned tilingName radius
        when has_been_scanned . liftIO $ do
            putStrLn "The lattice for this tiling and radius has already been completely scanned."
            exitSuccess
        forM_ [start,start+skip..tilingNumberOfLabelings] $ \n →
            let labeling = decodeLatticeLabeling tilingNumberOfOrientations tilingNumberOfRays n
                minimal_labeling =
                    minimum
                    .
                    map (permuteLatticeLabeling labeling)
                    $
                    tilingSymmetries
            in if minimal_labeling < labeling then return () else do
                solution <- liftIO $ solveFor labeling
                case solutionLogicalQubitDistances solution of
                    [] → return ()
                    distances@(d:_) → if d <= 2 then return () else do
                        liftIO . putStrLn $
                            show n
                            ++ " -> " ++
                            (show . collectDistances) distances
                        withTransaction ReadUncommitted $
                            storeSolution tilingName radius n solution
        markAsScanned tilingName radius
-- @-node:gcross.20100727222803.1685:@thin scan.hs
-- @-leo

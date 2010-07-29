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

import Data.IORef
import Data.List
import Data.Maybe

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
              | offset < 0 → do
                    putStrLn "The offset cannot be negative."
                    exitFailure
              | offset >= skip → do
                    putStrLn "The offset must be less than the skip."
                    exitFailure
              | otherwise → return offset
            _ → do
                putStrLn "The offset must be an integer."
                exitFailure
    return (tiling,radius,skip,offset)
-- @-node:gcross.20100727222803.1689:getArguments
-- @+node:gcross.20100728211848.1648:main
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
        checkIfScanned tilingName radius >>=
            (flip when . liftIO $ do
                putStrLn "The lattice for this tiling and radius has already been completely scanned."
                exitSuccess
            )
        when (skip > 1) $ do
            checkIfSkipperScanned tilingName radius skip offset >>=
                (flip when . liftIO $ do
                    putStrLn $ "The lattice for this tiling and radius has already been skip scanned for offset " ++ show offset
                    exitSuccess
                )
            lattice_skip ← fmap (fromMaybe skip) $ fetchLatticeSkip tilingName radius
            when (skip /= lattice_skip) . liftIO $ do
                putStrLn (printf "The skip for this lattice has already been established as %i, not %i." lattice_skip skip)
                exitFailure
        start ← fmap (fromMaybe offset) $ fetchCheckpoint tilingName radius skip offset
        when (start > offset) . liftIO $
            putStrLn $ "Starting from checkpoint " ++ show start ++ "..."
        counter_ref ← liftIO $ newIORef 0
        forM_ [start,start+skip..tilingNumberOfLabelings] $ \n → do
            let labeling = decodeLatticeLabeling tilingNumberOfOrientations tilingNumberOfRays n
                minimal_labeling =
                    minimum
                    .
                    map (permuteLatticeLabeling labeling)
                    $
                    tilingSymmetries
                checkpoint = setCheckpoint tilingName radius skip offset (n+skip)
            when (minimal_labeling >= labeling) $ do
                solution <- liftIO $ solveFor labeling
                case solutionLogicalQubitDistances solution of
                    [] → do
                        counter ← liftIO $ readIORef counter_ref
                        if counter == 100
                            then do
                                  checkpoint
                                  liftIO $ do
                                      putStrLn ("Examining " ++ show (n+skip) ++ "...")
                                      writeIORef counter_ref 0
                            else liftIO $ writeIORef counter_ref (counter+1)
                    distances@(d:_) → if d <= 2 then return () else do
                        liftIO . putStrLn $
                            show n
                            ++ " -> " ++
                            (show . collectDistances) distances
                        withTransaction ReadUncommitted $ do
                            storeSolution tilingName radius n solution
                            checkpoint
                        liftIO $ writeIORef counter_ref 0
        if skip == 1
            then markAsScanned tilingName radius
            else withTransaction ReadUncommitted $
                    markAsSkipperScanned tilingName radius skip offset
-- @-node:gcross.20100728211848.1648:main
-- @-node:gcross.20100727222803.1688:Functions
-- @-others
-- @-node:gcross.20100727222803.1685:@thin scan.hs
-- @-leo

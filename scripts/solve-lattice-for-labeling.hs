-- @+leo-ver=4-thin
-- @+node:gcross.20100316133702.1488:@thin solve-lattice-for-labeling.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100316133702.1489:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100316133702.1489:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100316133702.1490:<< Import needed modules >>
import Control.Exception
import Control.Monad

import Data.Array.Storable
import Data.IORef
import Data.Maybe

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import System.Environment
import System.IO

import Text.Printf

import CodeLattice
import CodeLattice.Database
import CodeLattice.Scanning
import CodeLattice.Tilings
-- @-node:gcross.20100316133702.1490:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100316133702.1491:main
main = do
    [tiling_name,growth_iteration_number_as_string,labeling_as_string] <- getArgs
    (number_of_orientations,number_of_rays,lattice) <-
        makeConnection "reader"
        >>=
        \connection ->
            withSession connection $ do
                (number_of_orientations,number_of_rays) <-
                    fmap (fromMaybe $ error "Can't find a tiling with this name!") $
                        doQuery
                            (sql $ "select number_of_orientations,number_of_rays from tilings where tiling_name = '" ++ tiling_name ++ "'" ++ ";")
                            get2
                            Nothing
                lattice <-
                    doQuery
                        (sql $ "select lattice_id from lattices where tiling_name = '" ++ tiling_name ++ "' and growth_iteration_number = " ++ growth_iteration_number_as_string ++ ";")
                        get1
                        Nothing
                    >>=
                    maybe (error "Can't find a lattice with this tiling and growth iteration number!") fetchLattice
                return (number_of_orientations,number_of_rays,lattice)
    let config = latticeToScanConfiguration number_of_orientations number_of_rays lattice
        number_of_qubits = scanNumberOfQubits config
        number_of_operators = scanNumberOfOperators config
    putStrLn $ show number_of_qubits ++ " qubits"
    putStrLn $ show number_of_operators ++ " operators"
    putStrLn $ "Solving for labeling " ++ labeling_as_string ++ "..."
    solveNoisilyForLabeling config (read labeling_as_string)
    return ()
-- @-node:gcross.20100316133702.1491:main
-- @-others
-- @-node:gcross.20100316133702.1488:@thin solve-lattice-for-labeling.hs
-- @-leo

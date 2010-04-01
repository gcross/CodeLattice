-- @+leo-ver=4-thin
-- @+node:gcross.20100331165456.1562:@thin generate-periodic-lattices.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100331165456.1563:<< Language extensions >>
-- @-node:gcross.20100331165456.1563:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100331165456.1564:<< Import needed modules >>
import Control.Monad
import Control.Monad.Trans

import Database.Enumerator

import CodeLattice
import CodeLattice.Database
import CodeLattice.Tilings
-- @-node:gcross.20100331165456.1564:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100331165456.1565:main
main =
    let lattices =
            concatMap (
                \tiling_name ->
                    drop 1
                    .
                    zip3 (repeat tiling_name) [0..]
                    .
                    fst
                    .
                    fst
                    .
                    fst
                    .
                    runLatticeMonadForTiling tiling_name
                    $
                    iteratePeriodicLatticeRepeatedly [originRawVertex] 6                 
            )
            .
            map tilingName
            $
            tilings
    in  makeConnection "builder"
        >>=
        \connection ->
            withSession connection $
                withTransaction ReadUncommitted $
                    forM_ lattices $ 
                        \(tiling_name,growth_iteration_number,lattice) ->
                            (liftIO $ do
                                putStrLn $
                                    "Storing tiling "
                                    ++ show tiling_name ++
                                    ", growth iteration #"
                                    ++ show growth_iteration_number ++
                                    "..."
                                putStrLn . drawLattice $ lattice
                            )
                            >>
                            storeLattice tiling_name True growth_iteration_number lattice
                            >>=
                            fetchLattice
                            >>=
                            flip unless (error "Lattices not equal!") . (== lattice)
-- @-node:gcross.20100331165456.1565:main
-- @-others
-- @-node:gcross.20100331165456.1562:@thin generate-periodic-lattices.hs
-- @-leo

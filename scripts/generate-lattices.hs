-- @+leo-ver=4-thin
-- @+node:gcross.20100312220352.1841:@thin generate-lattices.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100312220352.1846:<< Language extensions >>
-- @-node:gcross.20100312220352.1846:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100312220352.1843:<< Import needed modules >>
import Control.Monad
import Control.Monad.Trans

import Database.Enumerator

import CodeLattice
import CodeLattice.Database
import CodeLattice.Tilings
-- @-node:gcross.20100312220352.1843:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100312220352.1842:main
main =
    let lattices =
            concat
            [   let (((key_space_lattices,_),_),[x_map,y_map,o_map]) =
                        runLatticeMonadForTiling tiling_name $
                            iterateLatticeRepeatedly [originRawVertex] 20
                    position_space_lattices =
                        map (mapKeysToPositionsInLattice x_map y_map o_map)
                            key_space_lattices
                in zip3 (repeat tiling_name) [1..] position_space_lattices
            | tiling_name <- map tilingName tilings
            ]
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
                            storeLattice tiling_name growth_iteration_number lattice
                            >>=
                            fetchLattice
                            >>=
                            flip unless (error "Lattices not equal!") . (== lattice)
-- @-node:gcross.20100312220352.1842:main
-- @-others
-- @-node:gcross.20100312220352.1841:@thin generate-lattices.hs
-- @-leo

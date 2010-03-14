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
            fst
            .
            fst
            .
            fst
            .
            runLatticeMonadForTiling "quadrille" $
                iterateLatticeRepeatedly [originRawVertex] 20
    in  makeConnection "builder"
        >>=
        \connection ->
            withSession connection $
                withTransaction ReadUncommitted $
                    forM_ (zip [1..] lattices) $ 
                        \(growth_iteration_number,lattice) ->
                            (liftIO $ do
                                putStrLn $ "Storing iteration " ++ show growth_iteration_number ++ "..."
                                putStrLn . drawLattice $ lattice
                            )
                            >>
                            storeLattice "quadrille" growth_iteration_number lattice
                            >>=
                            fetchLattice
                            >>=
                            flip unless (error "Lattices not equal!") . (== lattice)
-- @-node:gcross.20100312220352.1842:main
-- @-others
-- @-node:gcross.20100312220352.1841:@thin generate-lattices.hs
-- @-leo

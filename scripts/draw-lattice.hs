-- @+leo-ver=4-thin
-- @+node:gcross.20100312220352.1851:@thin draw-lattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100312220352.1852:<< Language extensions >>
-- @-node:gcross.20100312220352.1852:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100312220352.1853:<< Import needed modules >>
import Control.Monad

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import System.Environment

import CodeLattice
import CodeLattice.Database
import CodeLattice.Tilings
-- @-node:gcross.20100312220352.1853:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100312220352.1854:main
main =
    getArgs
    >>=
    \(tiling_name:growth_iteration_number:_) -> (
        makeConnection "builder"
        >>=
        (\connection ->
            withSession connection $
                doQuery
                    (sql $ "select lattice_id from lattices where tiling_name = '" ++ tiling_name ++ "' and growth_iteration_number = " ++ growth_iteration_number ++ ";")
                    get1
                    Nothing
                >>=
                maybe (error "Can't find this lattice!") fetchLattice
        )
    )
    >>=
    putStrLn . drawLattice
-- @-node:gcross.20100312220352.1854:main
-- @-others
-- @-node:gcross.20100312220352.1851:@thin draw-lattice.hs
-- @-leo

-- @+leo-ver=4-thin
-- @+node:gcross.20100312220352.1851:@thin draw-lattice.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100312220352.1852:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100312220352.1852:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100312220352.1853:<< Import needed modules >>
import Control.Monad
import Control.Monad.Trans

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import System.Environment

import Text.Printf

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
    \(tiling_name:periodic:growth_iteration_number:_) → (
        makeConnection "builder"
        >>=
        (\connection →
            withSession connection $
                doQuery
                    (sql $ printf "select lattice_id from lattices where tiling_name = '%s' and periodic = %s and growth_iteration_number = %s;" tiling_name periodic growth_iteration_number)
                    get1
                    Nothing
                >>=
                maybe (error "Can't find this lattice!") (
                    \lattice_id →
                        liftIO (putStrLn $ "Lattice ID:" ++ lattice_id)
                        >>
                        fetchLattice lattice_id
                )
        )
    )
    >>=
    putStrLn . drawLattice
-- @nonl
-- @-node:gcross.20100312220352.1854:main
-- @-others
-- @-node:gcross.20100312220352.1851:@thin draw-lattice.hs
-- @-leo

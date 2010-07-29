-- @+leo-ver=4-thin
-- @+node:gcross.20100728132013.2004:@thin count-physical-qubits-for-everything.hs
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
-- @-others

main =
    fmap (read . head) getArgs
    >>=
    \maximum_radius →
        putStrLn
        .
        show
        .
        sort
        .
        nub
        $
        [ Set.size
          .
          latticeVertices
          $
          generatePeriodicLatticeForTiling tiling radius
        | tiling@Tiling{..} ← tilings
        , radius ← [1..maximum_radius]
        , not (tilingName == "deltille" && radius `mod` 3 == 0)
        ]
-- @-node:gcross.20100728132013.2004:@thin count-physical-qubits-for-everything.hs
-- @-leo

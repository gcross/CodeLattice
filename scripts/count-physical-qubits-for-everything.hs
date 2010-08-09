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
-- @+node:gcross.20100806120341.1671:main
main =
    fmap (read . head) getArgs
    >>=
    \maximum_number_of_physical_qubits →
        putStrLn
        .
        show
        .
        sort
        .
        nub
        .
        concat
        .
        map (
            takeWhile (<= maximum_number_of_physical_qubits)
            .
            (\tiling@Tiling{..} →
                [ Set.size
                  .
                  latticeVertices
                  .
                  generatePeriodicLatticeForTiling tiling
                  $
                  radius
                | radius ← [1..]
                , radius `mod` 3 /= 0 || tilingName /= "deltille"
                ]
            )
        )
        $
        tilings
-- @-node:gcross.20100806120341.1671:main
-- @-others
-- @-node:gcross.20100728132013.2004:@thin count-physical-qubits-for-everything.hs
-- @-leo

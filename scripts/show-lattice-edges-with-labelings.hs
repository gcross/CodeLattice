-- @+leo-ver=4-thin
-- @+node:gcross.20100316133702.1651:@thin show-lattice-edges-with-labelings.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100316133702.1652:<< Language extensions >>
-- @-node:gcross.20100316133702.1652:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100316133702.1653:<< Import needed modules >>
import Control.Arrow
import Control.Monad
import Control.Monad.Trans

import qualified Data.Bimap as Bimap
import qualified Data.IntMap as IntMap
import Data.Maybe
import qualified Data.Set as Set

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import System.Environment

import Text.Printf

import CodeLattice
import CodeLattice.Database
import CodeLattice.Tilings
-- @-node:gcross.20100316133702.1653:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100316133702.1654:main
main = do
    [tiling_name,growth_iteration_number_as_string,labeling_as_string] <- getArgs
    let labeling = read labeling_as_string
    (number_of_orientations,lattice) <-
        makeConnection "reader"
        >>=
        \connection ->
            withSession connection $ do
                number_of_orientations <-
                    fmap (fromMaybe $ error "Can't find a tiling with this name!") $
                        doQuery
                            (sql $ "select number_of_orientations from tilings where tiling_name = '" ++ tiling_name ++ "'" ++ ";")
                            get1
                            Nothing
                lattice <-
                    doQuery
                        (sql $ "select lattice_id from lattices where tiling_name = '" ++ tiling_name ++ "' and growth_iteration_number = " ++ growth_iteration_number_as_string ++ ";")
                        get1
                        Nothing
                    >>=
                    maybe (error "Can't find a lattice with this tiling and growth iteration number!") fetchLattice
                return (number_of_orientations,lattice)
    mapM (\((orientation_1,label_1),(orientation_2,label_2)) ->
            putStrLn $ printf "%i|%c --> %i|%c" orientation_1 label_1 orientation_2 label_2
        )
        .
        Set.toList
        .
        Set.fromList
        $
        let vertex_orientation_map =
                IntMap.fromAscList
                .
                map (second vertexOrientation)
                .
                Bimap.toAscList
                .
                latticeVertices
                $
                lattice
            label_to_char_map = [undefined,'X','Y','Z'] -- '
            lookupOrientationAndLabeling (EdgeSide vertex_number ray_number) =
                let orientation = fromJust (IntMap.lookup vertex_number vertex_orientation_map)
                    index = ray_number * number_of_orientations + orientation
                in (orientation,label_to_char_map !! (labeling !! index))
        in  (
                [(lookupOrientationAndLabeling side_1
                 ,lookupOrientationAndLabeling side_2
                 )
                | Edge side_1 side_2 <- latticeEdges lattice
                ]
                >>=
                \(a,b) -> [(a,b),(b,a)]
            )
-- @-node:gcross.20100316133702.1654:main
-- @-others
-- @-node:gcross.20100316133702.1651:@thin show-lattice-edges-with-labelings.hs
-- @-leo

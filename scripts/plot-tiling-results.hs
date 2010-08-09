-- @+leo-ver=4-thin
-- @+node:gcross.20100809113206.1715:@thin plot-tiling-results.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100809113206.1716:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100809113206.1716:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100809113206.1717:<< Import needed modules >>
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer.Lazy

import Data.Function
import Data.List
import Data.Maybe
import Data.Tuple.Select

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import Graphics.Rendering.Diagrams as D

import System.Environment
import System.Exit
import System.IO

import Text.Printf
import Text.Read

import CodeLattice.Database
import CodeLattice.Tilings
-- @-node:gcross.20100809113206.1717:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100809113206.1718:Functions
-- @+node:gcross.20100809113206.1719:getArguments
getArguments :: IO (Tiling,String)
getArguments = do
    args ← getArgs
    (tiling_name,filename) ←
        case args of
            [x,y] → return (x,y)
            _ → do
                putStrLn "Usage:  plot-tiling-results <tiling> <output filename.pdf>"
                exitFailure
    tiling ←
        case find ((== tiling_name) . tilingName) tilings of
            Just tiling → return tiling
            _ → do
                putStrLn "Tiling must be one of the following:"
                mapM_ (putStrLn . ('\t':) . tilingName) tilings -- '
                exitFailure
    return (tiling,filename)
-- @-node:gcross.20100809113206.1719:getArguments
-- @+node:gcross.20100809113206.1720:drawPolyAt
drawPolyAt :: Double → Int → Double → Double → Diagram
drawPolyAt base_radius n cx cy =
    position . (:[]) . ((,) (cx,cy)) $ rotRegPoly n (scaling*base_radius) rotation
  where
    rotation = rotation_in_degrees / 360
    radius = scaling * base_radius
    (scaling,rotation_in_degrees) = (!! (n-3))
        [(1.0,90)
        ,(1.3,45)
        ,(1.2,-90)
        ,(1.3,0)
        ]
-- @-node:gcross.20100809113206.1720:drawPolyAt
-- @+node:gcross.20100809113206.1722:main
main = do
    (tiling@Tiling{..},filename) ← getArguments
    connection ← makeConnection "reader"
    plotlines ←
        fmap (
            nub
            .
            map (map (\(_,x,y,z) → (x,y,z)))
            .
            groupBy ((==) `on` sel1)
        )
        .
        withSession connection
        $
        query
            (sql $ printf "select labeling, radius, distance, number_of_qubits from distances_of_scanned where tiling = '%s' and distance >= 3 order by labeling, radius desc, distance desc;" tilingName)
            fetch4
            ([] :: [(Int,Int,Int,Int)])
            "Error fetching results from the database:"
    renderAs PDF filename Auto . D.union $ [drawPolyAt 100 n 0 0 | n <- [3..6]]
-- @-node:gcross.20100809113206.1722:main
-- @-node:gcross.20100809113206.1718:Functions
-- @-others
-- @-node:gcross.20100809113206.1715:@thin plot-tiling-results.hs
-- @-leo

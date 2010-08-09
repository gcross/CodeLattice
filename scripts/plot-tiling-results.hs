-- @+leo-ver=4-thin
-- @+node:gcross.20100808143551.1695:@thin plot-tiling-results.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100808143551.1696:<< Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100808143551.1696:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100808143551.1697:<< Import needed modules >>
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer.Lazy

import Data.Function
import Data.List
import Data.Maybe
import Data.Tuple.Select

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import System.Environment
import System.Exit
import System.IO

import Text.PrettyPrint.HughesPJ
import Text.Printf
import Text.Read

import CodeLattice.Database
import CodeLattice.Tilings
-- @-node:gcross.20100808143551.1697:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100808143551.1698:Functions
-- @+node:gcross.20100808143551.1699:getArguments
getArguments :: IO (Tiling,Handle)
getArguments = do
    args ← getArgs
    (tiling_name,maybe_filename) ←
        case args of
            [x] → return (x,Nothing)
            [x,y] → return (x,Just y)
            _ → do
                putStrLn "Usage:  plot-tiling-results <tiling> [output filename]"
                exitFailure
    tiling ←
        case find ((== tiling_name) . tilingName) tilings of
            Just tiling → return tiling
            _ → do
                putStrLn "Tiling must be one of the following:"
                mapM_ (putStrLn . ('\t':) . tilingName) tilings -- '
                exitFailure
    handle ← maybe (return stdout) (flip openFile WriteMode) maybe_filename
    return (tiling,handle)
-- @-node:gcross.20100808143551.1699:getArguments
-- @+node:gcross.20100809113206.1679:drawPolyAt
drawPolyAt :: Double → Int → Double → Double → Doc
drawPolyAt base_radius n cx cy = hsep
    [text "gsave newpath"
    ,first_point <+> text "moveto"
    ,hsep [point <+> text "lineto" | point ← rest_points]
    ,text "closepath stroke grestore"
    ]
  where
    (first_point:rest_points) =
        [let angle = (rotation_in_degrees + fromIntegral i * polygon_angle)/180*pi
             x = cx + radius * cos angle
             y = cy + radius * sin angle
         in double x <+> double y
        | i ← [0..n-1]
        ]
    radius = scaling * base_radius
    (scaling,rotation_in_degrees) = (!! (n-3))
        [(1.0,90)
        ,(1.2,45)
        ,(1.4,-90)
        ,(1.6,0)
        ]
    polygon_angle = fromIntegral (360 `div` n)
-- @-node:gcross.20100809113206.1679:drawPolyAt
-- @+node:gcross.20100809113206.1688:setPageSize
setPageSize :: Int → Int → Doc
setPageSize width height = hsep
    [text "<< /PageSize ["
    ,int width
    ,int height
    ,text "] >> setpagedevice"
    ]

-- @-node:gcross.20100809113206.1688:setPageSize
-- @+node:gcross.20100808143551.1700:main
main = do
    (tiling@Tiling{..},handle) ← getArguments
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
    hPutStrLn handle . renderStyle (style { mode = LeftMode }) . vcat $
        [text "%!PS-Adobe-3.0"
        ,setPageSize 400 400
        ,vcat [drawPolyAt 100 i 200 200 | i <- [3..6]]
        ,text "showpage"
        ]
    hFlush handle
    hClose handle
-- @-node:gcross.20100808143551.1700:main
-- @-node:gcross.20100808143551.1698:Functions
-- @-others
-- @-node:gcross.20100808143551.1695:@thin plot-tiling-results.hs
-- @-leo

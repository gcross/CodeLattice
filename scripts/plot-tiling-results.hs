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
import Text.Read (reads)

import CodeLattice.Database
import CodeLattice.Tilings
-- @-node:gcross.20100808143551.1697:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100809113206.2150:Types
-- @+node:gcross.20100809113206.2151:Frame
data Frame = Frame
    {   frameXLabel :: String
    ,   frameXTics :: Tics
    ,   frameYLabel :: String
    ,   frameYTics :: Tics
    ,   frameLeftMargin :: Double
    ,   frameRightMargin :: Double
    ,   frameTopMargin :: Double
    ,   frameBottomMargin :: Double
    }
-- @-node:gcross.20100809113206.2151:Frame
-- @+node:gcross.20100809113206.2152:Tics
data Tics = Tics
    {   ticStart :: Int
    ,   ticDelta :: Int
    ,   ticEnd :: Int
    ,   ticCount :: Int
    ,   ticSize :: Double
    }
-- @-node:gcross.20100809113206.2152:Tics
-- @-node:gcross.20100809113206.2150:Types
-- @+node:gcross.20100809113206.2155:Prologue
prologue = vcat
    [text "%!PS-Adobe-3.0"
    ,text "/cshow {"
    ,nest 4 . vcat . map text $
        ["/angle exch def"
        ,"/text exch def"
        ,"gsave"
        ,"newpath"
        ,"0 0 moveto"
        ,"text false charpath pathbbox"
        ,"grestore"
        ,"2 index sub 2 div neg /ty exch def"
        ,"2 index sub 2 div neg /tx exch def"
        ,"pop pop"
        ,"gsave"
        ,"angle rotate"
        ,"tx ty rmoveto"
        ,"text show"
        ,"grestore"
        ]
    ,text "} def"
    ,text "/rshow {"
    ,nest 4 . vcat . map text $
        ["/text exch def"
        ,"gsave"
        ,"newpath"
        ,"0 0 moveto"
        ,"text false charpath pathbbox"
        ,"grestore"
        ,"2 index sub 2 div neg /ty exch def"
        ,"2 index sub neg /tx exch def"
        ,"pop pop"
        ,"gsave"
        ,"tx ty rmoveto"
        ,"text show"
        ,"grestore"
        ]
    ,text "} def"
    ]
-- @-node:gcross.20100809113206.2155:Prologue
-- @+node:gcross.20100808143551.1698:Functions
-- @+node:gcross.20100809113206.2159:computeYTics
computeYTics :: String → Tics
computeYTics "quadrille" = makeTics 100 1 2
computeYTics "isosnub quadrille" = makeTics 100 1 2
computeYTics "rhombihexadeltille" = makeTics 100 2 20
computeYTics _ = makeTics 100 20 200
-- @-node:gcross.20100809113206.2159:computeYTics
-- @+node:gcross.20100809113206.2148:drawFrame
drawFrame :: String → Frame → Doc
drawFrame title Frame{..} = vcat
    [(setPageSize `on` ceiling) page_width page_height
    ,int 4 <+> text "setlinewidth"
    ,drawFrameBox
    ,int 2 <+> text "setlinewidth"
    ,text "/TimesRoman findfont 36 scalefont setfont"
    ,drawXTics
    ,drawYTics
    ,text "/TimesRoman findfont 48 scalefont setfont"
    ,hsep
        [double (frameLeftMargin + width/2)
        ,double (y-128)
        ,text "moveto"
        ,(parens . text) frameXLabel
        ,int 0
        ,text "cshow"
        ]
    ,hsep
        [double (x-128)
        ,double (frameBottomMargin + height/2)
        ,text "moveto"
        ,(parens . text) frameYLabel
        ,int 90
        ,text "cshow"
        ]
    ,text "/TimesRoman findfont 48 scalefont setfont"
    ,hsep
        [double (frameLeftMargin + width/2)
        ,double (frameBottomMargin + height + 72)
        ,text "moveto"
        ,(parens . text) title
        ,int 0
        ,text "cshow"
        ]
    ]
  where
    x = frameLeftMargin
    y = frameBottomMargin
    width = ticsTotalSize frameXTics
    height = ticsTotalSize frameYTics
    page_width = frameLeftMargin + width + frameRightMargin
    page_height = frameBottomMargin + height + frameTopMargin
    drawFrameBox = vcat
        [text "newpath"
        ,nest 4 . vcat $
            [double x <+> double y <+> text "moveto"
            ,double (x+width) <+> double y <+> text "lineto"
            ,double (x+width) <+> double (y+height) <+> text "lineto"
            ,double x <+> double (y+height) <+> text "lineto"
            ,text "closepath"
            ]
        ,text "stroke"
        ]
    tic_size = 20
    drawXTics = vcat
        [ let x = ((+ frameLeftMargin) . (* ticSize) . fromIntegral) i
              label = ((+ ticStart) . (* ticDelta)) i
          in hsep
            [text "newpath"
            ,double x <+> double (y-tic_size/2) <+> text "moveto"
            ,double x <+> double (y+tic_size/2) <+> text "lineto"
            ,text "stroke"
            ,double x <+> double (y-36) <+> text "moveto" <+> (parens . int) label <+> int 0 <+> text "cshow"
            ]
        | i ← [0..ticCount-1]
        ]
      where
        Tics{..} = frameXTics
    drawYTics = vcat
        [ let y = ((+ frameBottomMargin) . (* ticSize) . fromIntegral) i
              label = ((+ ticStart) . (* ticDelta)) i
          in hsep
            [text "newpath"
            ,double (x-tic_size/2) <+> double y <+> text "moveto"
            ,double (x+tic_size/2) <+> double y <+> text "lineto"
            ,text "stroke"
            ,double (x-24) <+> double y <+> text "moveto" <+> (parens . int) label <+> text "rshow"
            ]
        | i ← [0..ticCount-1]
        ]
      where
        Tics{..} = frameYTics
-- @-node:gcross.20100809113206.2148:drawFrame
-- @+node:gcross.20100809113206.1679:drawPolyAt
drawPolyAt :: Int → Double → Double → Doc
drawPolyAt n cx cy = hsep
    [text "gsave"
    ,int 4 <+> text "setlinewidth"
    ,text "newpath"
    ,nest 4 . vcat $
        (first_point <+> text "moveto"
        :[point <+> text "lineto" | point ← rest_points]
        )
    ,text "closepath stroke"
    ,text "newpath"
    ,nest 4 $
        double cx <+> double cy <+> text "1 0 360 arc"
    ,text "closepath fill"
    ,text "grestore"
    ]
  where
    base_radius = 5
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
        ,(3.0,45)
        ,(3.0,-90)
        ,(4.0,0)
        ]
    polygon_angle = fromIntegral (360 `div` n)
-- @-node:gcross.20100809113206.1679:drawPolyAt
-- @+node:gcross.20100809113206.2158:drawDataPoint
drawDataPoint :: Frame → Int → Int → Int → Doc
drawDataPoint frame radius distance =
    uncurry (drawPolyAt distance) . getFramePoint frame radius
-- @-node:gcross.20100809113206.2158:drawDataPoint
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
-- @+node:gcross.20100809113206.2157:getFramePoint
getFramePoint :: Frame → Int → Int → (Double,Double)
getFramePoint Frame{..} x y =
    (let Tics{..} = frameXTics in frameLeftMargin + (fromIntegral (x - ticStart) / fromIntegral ticDelta) * ticSize
    ,let Tics{..} = frameYTics in frameBottomMargin + (fromIntegral (y - ticStart) / fromIntegral ticDelta) * ticSize
    )
-- @-node:gcross.20100809113206.2157:getFramePoint
-- @+node:gcross.20100809113206.2156:makeFrame
makeFrame :: String → Tics → String → Tics → Frame
makeFrame x_label x_tics y_label y_tics =
    Frame
    {   frameXTics = x_tics
    ,   frameXLabel = x_label
    ,   frameYTics = y_tics
    ,   frameYLabel = y_label
    ,   frameLeftMargin = 200
    ,   frameBottomMargin = 200
    ,   frameTopMargin = 150
    ,   frameRightMargin = 150
    }
-- @-node:gcross.20100809113206.2156:makeFrame
-- @+node:gcross.20100809113206.2153:makeTics
makeTics tic_size tics_delta tics_end =
    Tics
    {   ticStart = tics_start
    ,   ticDelta = tics_delta
    ,   ticEnd = tics_end
    ,   ticCount = (tics_end - tics_start) `div` tics_delta + 1
    ,   ticSize = tic_size
    }
  where
    tics_start = 0
-- @-node:gcross.20100809113206.2153:makeTics
-- @+node:gcross.20100809113206.1688:setPageSize
setPageSize :: Int → Int → Doc
setPageSize width height = hsep
    [text "<< /PageSize ["
    ,int width
    ,int height
    ,text "] >> setpagedevice"
    ]

-- @-node:gcross.20100809113206.1688:setPageSize
-- @+node:gcross.20100809113206.2154:ticsTotalSize
ticsTotalSize Tics{..} = ticSize * fromIntegral (ticCount-1)
-- @-node:gcross.20100809113206.2154:ticsTotalSize
-- @-node:gcross.20100808143551.1698:Functions
-- @+node:gcross.20100808143551.1700:main
main = do
    (tiling@Tiling{..},handle) ← getArguments
    connection ← makeConnection "reader"
    data_points ←
        withSession connection
        $
        query
            (sql $ printf "select distinct radius, distance, number_of_qubits from distances_of_scanned where tiling = '%s' and distance >= 3 order by radius desc, distance desc;" tilingName)
            fetch3
            ([] :: [(Int,Int,Int)])
            "Error fetching results from the database:"
    let frame =
            makeFrame
                "Radius"
                (makeTics 100 1 . (+1) . maximum . map sel1 $ data_points)
                "Number of qubits"
                (computeYTics tilingName)
    hPutStrLn handle . renderStyle (style { mode = LeftMode }) . vcat $
        [prologue
        ,drawFrame ("Codes found for the " ++ tilingName ++ " tiling") frame
        ,vcat
            [ drawDataPoint frame radius distance number_of_qubits
            | (radius,distance,number_of_qubits) ← data_points
            ]
        ,text "showpage"
        ]
    hFlush handle
    hClose handle
-- @-node:gcross.20100808143551.1700:main
-- @-others
-- @-node:gcross.20100808143551.1695:@thin plot-tiling-results.hs
-- @-leo

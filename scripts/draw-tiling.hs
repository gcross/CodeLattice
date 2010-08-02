-- @+leo-ver=4-thin
-- @+node:gcross.20100801112904.1624:@thin draw-tiling.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100801112904.1625:<< Language extensions >>
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100801112904.1625:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100801112904.1626:<< Import needed modules >>
import Control.Arrow

import Data.Eq.Approximate
import qualified Data.Foldable as Fold
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid

import System.Environment
import System.Exit
import System.IO

import Text.Printf

import CodeLattice
import CodeLattice.Periodic
import CodeLattice.Tilings
-- @-node:gcross.20100801112904.1626:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100801112904.1630:Types
-- @+node:gcross.20100801112904.1631:Min/Max
newtype Min a = Min { getMin :: Maybe a }
newtype Max a = Max { getMax :: Maybe a }
-- @-node:gcross.20100801112904.1631:Min/Max
-- @-node:gcross.20100801112904.1630:Types
-- @+node:gcross.20100801112904.1632:Instances
-- @+node:gcross.20100801112904.1633:Monoid (Min/Max)
instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    mappend (Min a) (Min b)
      | Just x ← a
      , Just y ← b
        = Min (Just (min x y))
      | otherwise
        = Min (getFirst $ (mappend `on` First) a b)

instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mappend (Max a) (Max b)
      | Just x ← a
      , Just y ← b
        = Max (Just (max x y))
      | otherwise
        = Max (getFirst $ (mappend `on` First) a b)
-- @-node:gcross.20100801112904.1633:Monoid (Min/Max)
-- @-node:gcross.20100801112904.1632:Instances
-- @+node:gcross.20100801112904.1627:Values
-- @+node:gcross.20100801112904.1628:prologue
prologue = unlines
    ["%!PS-Adobe-3.0"
    ,"/multiplier 100 def"
    ,"/setup_page {"
    ,"    /uy exch def"
    ,"    /ux exch def"
    ,"    /ly exch def"
    ,"    /lx exch def"
    ,"    /x_offset ux lx sub 2 div def"
    ,"    /y_offset uy ly sub 2 div def"
    ,"    << /PageSize ["
    ,"        ux lx sub multiplier mul"
    ,"        uy ly sub multiplier mul"
    ,"    ] >> setpagedevice"
    ,"} def"
    ,"/xcoord {x_offset add multiplier mul} def"
    ,"/ycoord {y_offset add multiplier mul} def"
    ,"/edge {"
    ,"    ycoord /y2 exch def"
    ,"    xcoord /x2 exch def"
    ,"    ycoord /y1 exch def"
    ,"    xcoord /x1 exch def"
    ,"    newpath"
    ,"    x1 y1 5 0 360 arc"
    ,"    fill"
    ,"    newpath"
    ,"    x2 y2 5 0 360 arc"
    ,"    fill"
    ,"    newpath"
    ,"    x1 y1 moveto"
    ,"    x2 y2 lineto"
    ,"    stroke"
    ,"} def"
    ]
-- @-node:gcross.20100801112904.1628:prologue
-- @-node:gcross.20100801112904.1627:Values
-- @+node:gcross.20100801112904.1629:Functions
-- @+node:gcross.20100801112904.2008:extractEdges
extractEdges :: Lattice → [((ApproximateDouble,ApproximateDouble),(ApproximateDouble,ApproximateDouble))]
extractEdges =
    map (((vertexLocationX &&& vertexLocationY) . edgeSideVertex . edgeLeftSide)
     &&& ((vertexLocationX &&& vertexLocationY) . edgeSideVertex . edgeRightSide)
    )
    .
    latticeEdges
-- @-node:gcross.20100801112904.2008:extractEdges
-- @+node:gcross.20100801112904.2012:getArguments
getArguments :: IO (Tiling,Int,Handle)
getArguments = do
    args ← getArgs
    (tiling_name,radius_as_string,maybe_filename) ←
        case args of
            [x,y] → return (x,y,Nothing)
            [x,y,z] → return (x,y,Just y)
            _ → do
                putStrLn "Usage:  draw-tiling <tiling> <radius> [output filename]"
                exitFailure
    tiling ←
        case find ((== tiling_name) . tilingName) tilings of
            Just tiling → return tiling
            _ → do
                putStrLn "Tiling must be one of the following:"
                mapM_ (putStrLn . ('\t':) . tilingName) tilings -- '
                exitFailure
    radius ←
        case reads radius_as_string of
            ((radius,_):_)
              | radius <= 0 → do
                    putStrLn "The radius must be greater than zero."
                    exitFailure
              | otherwise → return radius
            _ → do
                putStrLn "The radius must be an integer."
                exitFailure
    handle ← maybe (return stdout) (flip openFile WriteMode) maybe_filename
    return (tiling,radius,handle)
-- @-node:gcross.20100801112904.2012:getArguments
-- @+node:gcross.20100801112904.2013:applyWord
applyWord ::
    String →
    ((ApproximateDouble,ApproximateDouble),(ApproximateDouble,ApproximateDouble)) →
    String
applyWord word ((a,b),(c,d)) = unwords
    [show . unwrapAbsolutelyApproximateValue $ a
    ,show . unwrapAbsolutelyApproximateValue $ b
    ,show . unwrapAbsolutelyApproximateValue $ c
    ,show . unwrapAbsolutelyApproximateValue $ d
    ,word
    ]
-- @-node:gcross.20100801112904.2013:applyWord
-- @-node:gcross.20100801112904.1629:Functions
-- @+node:gcross.20100801112904.2010:main
main = do
    (tiling@Tiling{..},radius,handle) ← getArguments
    let Periodicity{..} = tilingPeriodicity
        lattice = generateLatticeForTiling tiling (fromIntegral (radius+1) * periodDistance)
        unwrapValue = unwrapAbsolutelyApproximateValue
        border@((hx,hy):border_tail) =
            periodicityComputeBorder
            .
            (* periodDistance)
            .
            fromIntegral
            $
            radius
        border_path = unlines $
            ["newpath"
            ,printf "\t%f xcoord %f ycoord moveto" (unwrapValue hx) (unwrapValue hy)
            ] ++ [printf "\t%f xcoord %f ycoord lineto" (unwrapValue x) (unwrapValue y) | (x,y) ← border_tail] ++
            ["closepath"
            ]
    hPutStrLn handle prologue
    hPutStrLn handle
        .
        applyWord "setup_page"
        .
        (((\x → x-1) *** (\x → x-1)) *** ((+1) *** (+1)))
        .
        ((minimum *** minimum) &&& (maximum *** maximum))
        .
        unzip
        $
        border
    hPutStrLn handle border_path
    hPutStrLn handle "clip"
    mapM_ (hPutStrLn handle . applyWord "edge") . extractEdges $ lattice
    hPutStrLn handle "initclip"
    hPutStrLn handle border_path
    hPutStrLn handle "5 setlinewidth stroke"
    hFlush handle
    hClose handle
-- @-node:gcross.20100801112904.2010:main
-- @-others
-- @-node:gcross.20100801112904.1624:@thin draw-tiling.hs
-- @-leo

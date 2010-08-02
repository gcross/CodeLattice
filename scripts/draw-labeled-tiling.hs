-- @+leo-ver=4-thin
-- @+node:gcross.20100801215024.1717:@thin draw-labeled-tiling.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100801215024.1718:<< Language extensions >>
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100801215024.1718:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100801215024.1719:<< Import needed modules >>
import Control.Arrow
import Control.Monad

import Data.Eq.Approximate
import qualified Data.Foldable as Fold
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid

import System.Environment
import System.Exit
import System.IO

import Text.Printf

import CodeLattice
import CodeLattice.Labeling
import CodeLattice.Periodic
import CodeLattice.Tilings
-- @-node:gcross.20100801215024.1719:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100801215024.1720:Types
-- @+node:gcross.20100801215024.1721:Min/Max
newtype Min a = Min { getMin :: Maybe a }
newtype Max a = Max { getMax :: Maybe a }
-- @-node:gcross.20100801215024.1721:Min/Max
-- @-node:gcross.20100801215024.1720:Types
-- @+node:gcross.20100801215024.1722:Instances
-- @+node:gcross.20100801215024.1723:Monoid (Min/Max)
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
-- @-node:gcross.20100801215024.1723:Monoid (Min/Max)
-- @-node:gcross.20100801215024.1722:Instances
-- @+node:gcross.20100801215024.1724:Values
-- @+node:gcross.20100801215024.1725:prologue
prologue = unlines
    ["%!PS-Adobe-3.0"
    ,"/Helvetica-Bold findfont 32 scalefont setfont"
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
    ,"/mod180 { dup 90 gt { 180 sub } if } def"
    ,"/centershow {"
    ,"    /angle exch def"
    ,"    /text exch def"
    ,"    gsave"
    ,"    newpath"
    ,"    0 0 moveto"
    ,"    text false charpath pathbbox"
    ,"    grestore"
    ,"    2 index sub 2 div neg /ty exch def"
    ,"    2 index sub 2 div neg /tx exch def"
    ,"    pop pop"
    ,"    gsave"
    ,"    angle rotate"
    ,"    tx ty rmoveto"
    ,"    text show"
    ,"    grestore"
    ,"} def"
    ,"/edge {"
    ,"    /p2 exch def"
    ,"    ycoord /y2 exch def"
    ,"    xcoord /x2 exch def"
    ,"    /p1 exch def"
    ,"    ycoord /y1 exch def"
    ,"    xcoord /x1 exch def"
    ,"    newpath"
    ,"        x1 y1 5 0 360 arc"
    ,"    fill"
    ,"    newpath"
    ,"        x2 y2 5 0 360 arc"
    ,"    fill"
    ,"    newpath"
    ,"        x1 y1 moveto"
    ,"        x2 y2 lineto"
    ,"    stroke"
    ,"    /dx x2 x1 sub 3 div def"
    ,"    /dy y2 y1 sub 3 div def"
    ,"    /a dy dx atan mod180 def"
    ,"    x1 y1 moveto"
    ,"    dx dy rmoveto"
    ,"    p1 a centershow"
    ,"    x2 y2 moveto"
    ,"    dx neg dy neg rmoveto"
    ,"    p2 a centershow"
    ,"} def"
    ]
-- @-node:gcross.20100801215024.1725:prologue
-- @-node:gcross.20100801215024.1724:Values
-- @+node:gcross.20100801215024.1726:Functions
-- @+node:gcross.20100801215024.1728:getArguments
getArguments :: IO (Tiling,Int,LatticeLabeling,Handle)
getArguments = do
    args ← getArgs
    (tiling_name,radius_as_string,labeling_as_string,maybe_filename) ←
        case args of
            [x,y,z] → return (x,y,z,Nothing)
            [x,y,z,w] → return (x,y,z,Just w)
            _ → do
                putStrLn "Usage:  draw-tiling <tiling> <radius> <labeling> [output filename]"
                exitFailure
    tiling@Tiling{..} ←
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
    labeling ←
        case reads labeling_as_string of
            ((labeling,_):_)
              | labeling < 0 → do
                    putStrLn "The labeling cannot be negative."
                    exitFailure
              | otherwise → return (decodeLatticeLabeling tilingNumberOfOrientations tilingNumberOfRays labeling)
            _ → do
                putStrLn "The labeling must be an integer."
                exitFailure
    handle ← maybe (return stdout) (flip openFile WriteMode) maybe_filename
    return (tiling,radius,labeling,handle)
-- @-node:gcross.20100801215024.1728:getArguments
-- @+node:gcross.20100801215024.1729:applyWord
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
-- @-node:gcross.20100801215024.1729:applyWord
-- @-node:gcross.20100801215024.1726:Functions
-- @+node:gcross.20100801215024.1730:main
main = do
    (tiling@Tiling{..},radius,labeling,handle) ← getArguments
    let Periodicity{..} = tilingPeriodicity
        lattice@Lattice{..} = generateLatticeForTiling tiling (fromIntegral (radius+1) * periodDistance)
        labeling_map = Map.fromList
            [ ((orientation,ray),pauli)
            | ray ← [0..tilingNumberOfRays], orientation ← tilingOrientations
            | pauli ← map ([undefined,'X','Z','Y'] !!) . flattenLatticeLabeling $ labeling -- '
            ]
        getPauliLabel orientation ray = fromJust (Map.lookup (orientation,ray) labeling_map)
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
    forM_ latticeEdges $ \(Edge (EdgeSide (Vertex x1 y1 o1) r1) (EdgeSide (Vertex x2 y2 o2) r2)) →
        hPutStrLn handle $ printf "%f %f (%c) %f %f (%c) edge"
            (unwrapAbsolutelyApproximateValue x1)
            (unwrapAbsolutelyApproximateValue y1)
            (getPauliLabel o1 r1)
            (unwrapAbsolutelyApproximateValue x2)
            (unwrapAbsolutelyApproximateValue y2)
            (getPauliLabel o2 r2)
    hPutStrLn handle "initclip"
    hPutStrLn handle border_path
    hPutStrLn handle "5 setlinewidth stroke"
    hFlush handle
    hClose handle
-- @-node:gcross.20100801215024.1730:main
-- @-others
-- @-node:gcross.20100801215024.1717:@thin draw-labeled-tiling.hs
-- @-leo

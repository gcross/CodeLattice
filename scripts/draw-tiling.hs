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

import CodeLattice
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
    ,"/edge {"
    ,"    y_offset add multiplier mul /y2 exch def"
    ,"    x_offset add multiplier mul /x2 exch def"
    ,"    y_offset add multiplier mul /y1 exch def"
    ,"    x_offset add multiplier mul /x1 exch def"
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
-- @+node:gcross.20100801112904.2009:isPeriodicEdge
isPeriodicEdge :: Edge → Bool
isPeriodicEdge (Edge (EdgeSide (Vertex x1 y1 _) _) (EdgeSide (Vertex x2 y2 _) _)) =
    sqrt ((x2 - x1)^2 + (y2 - y1)^2) /= 1
-- @-node:gcross.20100801112904.2009:isPeriodicEdge
-- @+node:gcross.20100801112904.1634:computePageSize
computePageSize :: Lattice → ((ApproximateDouble,ApproximateDouble),(ApproximateDouble,ApproximateDouble))
computePageSize =
    (((fromJust . getMin) *** (fromJust . getMin)) *** ((fromJust . getMax) *** (fromJust . getMax)))
    .
    Fold.foldMap (\(Vertex x y _) →
        ((Min (Just x),Min (Just y)),(Max (Just x),Max (Just y)))
    )
    .
    latticeVertices
-- @-node:gcross.20100801112904.1634:computePageSize
-- @+node:gcross.20100801112904.2008:extractNonPeriodicEdges
extractNonPeriodicEdges :: Lattice → [((ApproximateDouble,ApproximateDouble),(ApproximateDouble,ApproximateDouble))]
extractNonPeriodicEdges =
    map (((vertexLocationX &&& vertexLocationY) . edgeSideVertex . edgeLeftSide)
     &&& ((vertexLocationX &&& vertexLocationY) . edgeSideVertex . edgeRightSide)
    )
    .
    filter (not . isPeriodicEdge)
    .
    latticeEdges
-- @-node:gcross.20100801112904.2008:extractNonPeriodicEdges
-- @+node:gcross.20100801112904.2012:getArguments
getArguments :: IO (Tiling,Handle)
getArguments = do
    args ← getArgs
    (tiling_name,maybe_filename) ←
        case args of
            [x] → return (x,Nothing)
            [x,y] → return (x,Just y)
            _ → do
                putStrLn "Usage:  draw-tiling <tiling> [output filename]"
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
    (Tiling{..},handle) ← getArguments
    hPutStrLn handle prologue
    hPutStrLn handle . applyWord "setup_page" . computePageSize $ tilingUnitRadiusLattice
    mapM_ (hPutStrLn handle . applyWord "edge") . extractNonPeriodicEdges $ tilingUnitRadiusLattice
    hFlush handle
    hClose handle
-- @-node:gcross.20100801112904.2010:main
-- @-others
-- @-node:gcross.20100801112904.1624:@thin draw-tiling.hs
-- @-leo

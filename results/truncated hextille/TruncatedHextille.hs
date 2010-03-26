-- @+leo-ver=4-thin
-- @+node:gcross.20100316185958.1476:@thin TruncatedHextille.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100316185958.1477:<< Language extensions >>
-- @-node:gcross.20100316185958.1477:<< Language extensions >>
-- @nl

module TruncatedHextille where

-- @<< Import needed modules >>
-- @+node:gcross.20100316185958.1478:<< Import needed modules >>
import qualified Data.Set as Set

import Text.Printf
-- @-node:gcross.20100316185958.1478:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100316190653.1484:Types
-- @+node:gcross.20100316190653.1485:Edge(Side)
data Edge = Edge EdgeSide EdgeSide deriving (Eq,Ord)
data EdgeSide = EdgeSide Int Int Char deriving (Eq,Ord)
-- @-node:gcross.20100316190653.1485:Edge(Side)
-- @+node:gcross.20100316190653.1493:Direction
data Direction = L | R | U | D | UL | UR | DL | DR
-- @-node:gcross.20100316190653.1493:Direction
-- @-node:gcross.20100316190653.1484:Types
-- @+node:gcross.20100316190653.1491:Instances
-- @+node:gcross.20100316190653.1492:Show Edge
instance Show Edge where
    show (Edge (EdgeSide x1 y1 o1) (EdgeSide x2 y2 o2)) =
        printf "(%i,%i) %c (%i,%i) %c" x1 (-y1) o1 x2 (-y2) o2
-- @-node:gcross.20100316190653.1492:Show Edge
-- @-node:gcross.20100316190653.1491:Instances
-- @+node:gcross.20100316190653.1486:Values
-- @+node:gcross.20100316190653.1487:edges
edges =
    [Edge (EdgeSide 3 1 'Z') (EdgeSide 5 1 'Z')
    ,Edge (EdgeSide 5 1 'X') (EdgeSide 6 2 'X')
    ,Edge (EdgeSide 6 2 'Y') (EdgeSide 7 3 'X')
    ,Edge (EdgeSide 7 3 'X') (EdgeSide 7 4 'X')
    ,Edge (EdgeSide 7 4 'Z') (EdgeSide 6 5 'X')
    ,Edge (EdgeSide 6 5 'Y') (EdgeSide 5 6 'Y')
    ,Edge (EdgeSide 5 6 'X') (EdgeSide 3 6 'X')
    ,Edge (EdgeSide 3 6 'X') (EdgeSide 2 5 'X')
    ,Edge (EdgeSide 2 5 'Y') (EdgeSide 1 4 'Y')
    ,Edge (EdgeSide 1 4 'X') (EdgeSide 1 3 'X')
    ,Edge (EdgeSide 1 3 'Y') (EdgeSide 2 2 'Y')
    ,Edge (EdgeSide 2 2 'Y') (EdgeSide 3 1 'Y')
    ]
-- @+at
--      ++ -- diamond
--      [Edge (EdgeSide 3 1 'X') (EdgeSide 4 0 'Z')
--      ,Edge (EdgeSide 4 0 'Y') (EdgeSide 5 1 'Y')
--      ,Edge (EdgeSide 5 6 'Y') (EdgeSide 4 7 'Y')
--      ,Edge (EdgeSide 4 7 'X') (EdgeSide 3 6 'Y')
--      ]
-- @-at
-- @@c
-- @+at
--      ++ -- star
--      [Edge (EdgeSide 7 3 'Y') (EdgeSide 8 2 'Y')
--      ,Edge (EdgeSide 8 2 'X') (EdgeSide 6 2 'X')
--      ,Edge (EdgeSide 1 3 'X') (EdgeSide 0 2 'Y')
--      ,Edge (EdgeSide 0 2 'X') (EdgeSide 2 2 'X')
--      ,Edge (EdgeSide 7 4 'Y') (EdgeSide 8 5 'Y')
--      ,Edge (EdgeSide 8 5 'Z') (EdgeSide 6 5 'Z')
--      ,Edge (EdgeSide 2 5 'Z') (EdgeSide 0 5 'Z')
--      ,Edge (EdgeSide 0 5 'X') (EdgeSide 1 4 'Z')
--      ]
-- @-at
-- @@c
-- @+at
--      ++ -- half star 1
--      [Edge (EdgeSide 1 3 'X') (EdgeSide 0 2 'Y')
--      ,Edge (EdgeSide 0 2 'X') (EdgeSide 2 2 'X')
--      ,Edge (EdgeSide 7 4 'Y') (EdgeSide 8 5 'Y')
--      ,Edge (EdgeSide 8 5 'Z') (EdgeSide 6 5 'Z')
--      ]
-- @-at
-- @@c
-- @+at
--      ++ -- half star 2
--      [Edge (EdgeSide 7 3 'Y') (EdgeSide 8 2 'Y')
--      ,Edge (EdgeSide 8 2 'X') (EdgeSide 6 2 'X')
--      ,Edge (EdgeSide 2 5 'Z') (EdgeSide 0 5 'Z')
--      ,Edge (EdgeSide 0 5 'X') (EdgeSide 1 4 'Z')
--      ]
-- @-at
-- @@c
-- @-node:gcross.20100316190653.1487:edges
-- @-node:gcross.20100316190653.1486:Values
-- @+node:gcross.20100316190653.1488:Functions
-- @+node:gcross.20100316190653.1489:shiftEdge
shiftEdge :: Int -> Int -> Edge -> Edge
shiftEdge x_offset y_offset (Edge (EdgeSide x1 y1 o1) (EdgeSide x2 y2 o2)) =
    Edge (EdgeSide (x1+x_offset) (y1+y_offset) o1) (EdgeSide (x2+x_offset) (y2+y_offset) o2)
-- @-node:gcross.20100316190653.1489:shiftEdge
-- @+node:gcross.20100316190653.1490:shift
shift :: Direction -> [Edge] -> [Edge]
shift direction = map (shiftEdge x_offset y_offset)
  where
    (x_offset,y_offset) =
        case direction of
            L ->  (-6, 0)
            R ->  ( 6, 0)
            U ->  ( 0,-8)
            D ->  ( 0, 8)
            UL -> (-3,-4)
            UR -> ( 3,-4)
            DL -> (-3, 4)
            DR -> ( 3, 4)
-- @-node:gcross.20100316190653.1490:shift
-- @+node:gcross.20100316190653.1546:putEdges
putEdges :: [Edge] -> IO ()
putEdges =
    mapM_ (putStrLn . show)
    .
    Set.toList
    .
    Set.fromList
-- @-node:gcross.20100316190653.1546:putEdges
-- @-node:gcross.20100316190653.1488:Functions
-- @-others
-- @-node:gcross.20100316185958.1476:@thin TruncatedHextille.hs
-- @-leo

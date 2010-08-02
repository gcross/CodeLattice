-- @+leo-ver=4-thin
-- @+node:gcross.20100801215024.1652:@thin Periodic.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100801215024.1683:<< Language extensions >>
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100801215024.1683:<< Language extensions >>
-- @nl

module CodeLattice.Periodic where

-- @<< Import needed modules >>
-- @+node:gcross.20100801215024.1684:<< Import needed modules >>
import Control.Applicative
import Control.Arrow

import Data.Eq.Approximate
import Data.Function
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import CodeLattice
-- @-node:gcross.20100801215024.1684:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100801215024.1675:Types
-- @+node:gcross.20100801215024.1677:Periodicity
data Periodicity = Periodicity
    {   periodicityComputeVertexDistance :: Vertex → ApproximateDouble
    ,   periodicityWrapVertexAround :: ApproximateDouble → Vertex → Vertex
    ,   periodicityComputeBorder :: ApproximateDouble → [(ApproximateDouble,ApproximateDouble)]
    ,   periodDistance :: ApproximateDouble
    }
-- @-node:gcross.20100801215024.1677:Periodicity
-- @-node:gcross.20100801215024.1675:Types
-- @+node:gcross.20100801215024.1700:Functions
-- @+node:gcross.20100801215024.1693:Canonicalization
-- @+node:gcross.20100801215024.1695:canonicalizePeriodicEdge
canonicalizePeriodicEdge :: Periodicity → Int → Edge → Edge
canonicalizePeriodicEdge periodicity radius =
    canonicalizeEdge
    .
    liftA2 Edge
        (canonicalizePeriodicEdgeSide periodicity radius . edgeLeftSide)
        (canonicalizePeriodicEdgeSide periodicity radius . edgeRightSide)
-- @-node:gcross.20100801215024.1695:canonicalizePeriodicEdge
-- @+node:gcross.20100801215024.1696:canonicalizePeriodicEdgeSide
canonicalizePeriodicEdgeSide :: Periodicity → Int → EdgeSide → EdgeSide
canonicalizePeriodicEdgeSide periodicity radius (EdgeSide v r) =
    EdgeSide (canonicalizePeriodicVertex periodicity radius v) r
-- @-node:gcross.20100801215024.1696:canonicalizePeriodicEdgeSide
-- @+node:gcross.20100801215024.1697:canonicalizePeriodicLattice
canonicalizePeriodicLattice :: Periodicity → Int → Lattice → Lattice
canonicalizePeriodicLattice periodicity radius =
    latticeFromEdges
    .
    map (canonicalizePeriodicEdge periodicity radius)
    .
    latticeEdges
-- @-node:gcross.20100801215024.1697:canonicalizePeriodicLattice
-- @+node:gcross.20100801215024.1698:canonicalizePeriodicVertex
canonicalizePeriodicVertex :: Periodicity → Int → Vertex → Vertex 
canonicalizePeriodicVertex Periodicity{..} radius v
  | isBorder v = wrapAround v `min` v
  | otherwise  = v
  where
    wrap_around_distance = fromIntegral radius * periodDistance
    wrapAround = periodicityWrapVertexAround wrap_around_distance
    isBorder = (== wrap_around_distance) . periodicityComputeVertexDistance
-- @-node:gcross.20100801215024.1698:canonicalizePeriodicVertex
-- @+node:gcross.20100801215024.1699:canonicalizePeriodicVertices
canonicalizePeriodicVertices :: Periodicity → Int → Set Vertex → Set Vertex
canonicalizePeriodicVertices periodicity radius =
    Set.map (canonicalizePeriodicVertex periodicity radius)
-- @-node:gcross.20100801215024.1699:canonicalizePeriodicVertices
-- @-node:gcross.20100801215024.1693:Canonicalization
-- @+node:gcross.20100801215024.1678:Lattice
-- @+node:gcross.20100801215024.1682:growPeriodicLattice
growPeriodicLattice ::
    Periodicity →
    (ApproximateDouble → ApproximateDouble) →
    ApproximateDouble →
    Int →
    [Vertex] →
    LatticeMonad (Lattice,ApproximateDouble,[Vertex])
growPeriodicLattice
    periodicity@Periodicity{..}
    increaseDistance
    starting_distance
    desired_radius
    = go starting_distance
  where
    go current_distance current_vertices = do
        let withinBounds = (<= current_distance) . periodicityComputeVertexDistance
            next_distance = increaseDistance current_distance
        next_vertices ← growLatticeToBounds withinBounds current_vertices
        lattice ← getLattice
        case periodizeLattice periodicity desired_radius lattice of
            Just periodic_lattice → return (periodic_lattice,next_distance,next_vertices)
            Nothing → go next_distance next_vertices
-- @-node:gcross.20100801215024.1682:growPeriodicLattice
-- @+node:gcross.20100801215024.1680:periodizeLattice
periodizeLattice ::
    Periodicity →
    Int →
    Lattice →
    Maybe Lattice
periodizeLattice
    periodicity@Periodicity
        {   periodicityComputeVertexDistance = computeVertexDistance
        ,   periodicityWrapVertexAround = wrapVertexAround
        ,   periodDistance = translation_distance
        }
    requested_radius
    lattice@Lattice{..}
  | Just (maximum_distance,_) ←
        Set.maxView
        .
        Set.map computeVertexDistance
        $
        latticeVertices
  , maximum_distance >= translation_distance * fromIntegral (requested_radius+2)
     = Just $ let
        new_edges =
            Set.toList
            .
            Set.fromList
            .
            map (canonicalizePeriodicEdge periodicity requested_radius)
            .
            mapMaybe (\edge@(Edge s1@(EdgeSide v1 r1) s2@(EdgeSide v2 r2)) →
                let wrapped_v1 = wrapAround v1
                    wrapped_v2 = wrapAround v2
                in case (placeVertex v1,placeVertex v2) of
                    (GT,GT) → Nothing
                    (EQ,GT) → Nothing
                    (GT,EQ) → Nothing
                    (LT,LT) → Just edge
                    (EQ,EQ) → Just edge
                    (LT,EQ) → Just edge
                    (EQ,LT) → Just edge
                    (LT,GT) → Just (Edge s1 (EdgeSide wrapped_v2 r2))
                    (GT,LT) → Just (Edge (EdgeSide wrapped_v1 r1) s2)
            )
            $
            latticeEdges
          where
            wrap_around_distance = fromIntegral requested_radius * translation_distance
            placeVertex = (`compare` wrap_around_distance) . computeVertexDistance
            wrapAround = wrapVertexAround wrap_around_distance
       in latticeFromEdges new_edges
  | otherwise
     = Nothing
-- @-node:gcross.20100801215024.1680:periodizeLattice
-- @-node:gcross.20100801215024.1678:Lattice
-- @+node:gcross.20100801215024.1664:Periodicities
-- @+node:gcross.20100801215024.1665:makeComputeDistanceFrom
makeComputeDistanceFrom ::
    [(ApproximateDouble,ApproximateDouble)] →
    Vertex →
    ApproximateDouble
makeComputeDistanceFrom vectors (Vertex x y _) =
    maximum
    .
    map (\(bx,by) → abs (bx*x + by*y))
    $
    vectors
-- @-node:gcross.20100801215024.1665:makeComputeDistanceFrom
-- @+node:gcross.20100801215024.1666:makeReflectiveWrapAroundFrom
makeReflectiveWrapAroundFrom ::
    [(ApproximateDouble,ApproximateDouble)] →
    ApproximateDouble →
    Vertex →
    Vertex
makeReflectiveWrapAroundFrom vectors d vertex = foldl' makeReflectiveWrapAroundFromVector vertex vectors -- '
  where
    makeReflectiveWrapAroundFromVector vertex@(Vertex x y o) (bx,by)
      | abs r < d = vertex
      | otherwise  = Vertex (x + w * bx) (y + w * by) o
      where
        r = (bx*x + by*y)
        w = - 2 * signum r * d
-- @-node:gcross.20100801215024.1666:makeReflectiveWrapAroundFrom
-- @+node:gcross.20100801215024.1667:wrapVertexAroundVector
wrapVertexAroundVector ::
    (ApproximateDouble,ApproximateDouble) →
    ApproximateDouble →
    Vertex →
    Vertex
wrapVertexAroundVector (bx,by) d vertex@(Vertex x y o)
  | abs r < d = vertex
  | otherwise  = Vertex (x - w*bx) (y - w*by) o
 where
    r = bx*x + by*y
    w = 2 * signum r * d
-- @-node:gcross.20100801215024.1667:wrapVertexAroundVector
-- @+node:gcross.20100801215024.1668:rotate
rotate angle_in_degrees (x,y) =
    (x*cos_angle - y*sin_angle, y*cos_angle + x*sin_angle)
  where
    angle = angle_in_degrees / 180 * pi
    cos_angle = cos angle
    sin_angle = sin angle
-- @-node:gcross.20100801215024.1668:rotate
-- @+node:gcross.20100801215024.1669:rectangularPeriodicity
rectangularPeriodicity y_over_x = rectangularPeriodicityRotatedBy y_over_x 0
-- @-node:gcross.20100801215024.1669:rectangularPeriodicity
-- @+node:gcross.20100801215024.1670:rectangularPeriodicityRotatedBy
rectangularPeriodicityRotatedBy y_over_x angle distance =
    let [b1,b2@(b2x,b2y)] = map (rotate angle) [(1,0),(0,1)]

        computeDistanceFrom = makeComputeDistanceFrom [b1,(b2x/y_over_x,b2y/y_over_x)]

        wrapAround d =
            wrapVertexAroundVector b2 (d*y_over_x)
            .
            wrapVertexAroundVector b1 d

        computeBorder d =
            map (rotate angle . ((*d) *** (*d)))
            [(1,y_over_x)
            ,(-1,y_over_x)
            ,(-1,-y_over_x)
            ,(1,-y_over_x)
            ]

    in Periodicity computeDistanceFrom wrapAround computeBorder distance
-- @-node:gcross.20100801215024.1670:rectangularPeriodicityRotatedBy
-- @+node:gcross.20100801215024.1671:squarePeriodicity
squarePeriodicity = squarePeriodicityRotatedBy 0
-- @-node:gcross.20100801215024.1671:squarePeriodicity
-- @+node:gcross.20100801215024.1672:squarePeriodicityRotatedBy
squarePeriodicityRotatedBy = rectangularPeriodicityRotatedBy 1
-- @-node:gcross.20100801215024.1672:squarePeriodicityRotatedBy
-- @+node:gcross.20100801215024.1673:hexagonalPeriodicity
hexagonalPeriodicity = hexagonalPeriodicityRotatedBy 0
-- @nonl
-- @-node:gcross.20100801215024.1673:hexagonalPeriodicity
-- @+node:gcross.20100801215024.1674:hexagonalPeriodicityRotatedBy
hexagonalPeriodicityRotatedBy angle distance =
    let basis@[b1,b2,b3] = map (rotate angle)
            [(sqrt 3/2,1/2)
            ,(0,1)
            ,(-sqrt 3/2,1/2)
            ]

        computeDistanceFrom = makeComputeDistanceFrom basis

        wrapAround d vertex@(Vertex x y _)
          | offsetIs   0 = wrapVertexAroundVector b1 d vertex
          | offsetIs  60 = wrapVertexAroundVector b2 d vertex
          | offsetIs 120 = wrapVertexAroundVector b3 d vertex
          | otherwise    =
                error (
                    "Cannot wrap vertex around distance "
                    ++ show (unwrapAbsolutelyApproximateValue d) ++
                    " because it is located at "
                    ++ show (((,) `on` unwrapAbsolutelyApproximateValue) x y) ++
                    " @ "
                    ++ show (unwrapAbsolutelyApproximateValue $ atan2 y x / pi * 180) ++
                    ", which is on a (hexagonal) corner"
                )
          where
            vertex_angle = modulo360 (atan2 y x/pi*180 - angle)
            isInside a b = (a < vertex_angle) && (vertex_angle < b)
            offsetIs offset =
                isInside offset (offset+60)
             || isInside (offset+180) (offset+240)

        computeBorder d =
            map (rotate angle . ((*d) *** (*d)))
            [(2/sqrt 3,0)
            ,(1/sqrt 3,1)
            ,(-1/sqrt 3,1)
            ,(-2/sqrt 3,0)
            ,(-1/sqrt 3,-1)
            ,(1/sqrt 3,-1)
            ]

    in Periodicity computeDistanceFrom wrapAround computeBorder distance
-- @-node:gcross.20100801215024.1674:hexagonalPeriodicityRotatedBy
-- @-node:gcross.20100801215024.1664:Periodicities
-- @-node:gcross.20100801215024.1700:Functions
-- @-others
-- @-node:gcross.20100801215024.1652:@thin Periodic.hs
-- @-leo

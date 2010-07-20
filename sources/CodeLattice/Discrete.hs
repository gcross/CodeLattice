-- @+leo-ver=4-thin
-- @+node:gcross.20100717003017.2420:@thin Discrete.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100717003017.2421:<< Language extensions >>
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100717003017.2421:<< Language extensions >>
-- @nl

module CodeLattice.Discrete where

-- @<< Import needed modules >>
-- @+node:gcross.20100717003017.2422:<< Import needed modules >>
import Control.Applicative
import Control.Arrow

import Data.Char
import Data.Foldable (toList)
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import CodeLattice
-- @-node:gcross.20100717003017.2422:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100717003017.2423:Types
-- @+node:gcross.20100717003017.2428:DiscreteEdge
data DiscreteEdge = DiscreteEdge
    {   discreteEdgeLeftSide :: DiscreteEdgeSide
    ,   discreteEdgeRightSide :: DiscreteEdgeSide
    }
-- @-node:gcross.20100717003017.2428:DiscreteEdge
-- @+node:gcross.20100717003017.2426:DiscreteEdgeSide
data DiscreteEdgeSide = DiscreteEdgeSide
    {   discreteEdgeSideVertexNumber :: Int
    ,   discreteEdgeSideRayNumber :: Int
    }
-- @-node:gcross.20100717003017.2426:DiscreteEdgeSide
-- @+node:gcross.20100717003017.2424:DiscreteLattice
data DiscreteLattice = DiscreteLattice
    {   discreteLatticeVertices :: Seq DiscreteVertex
    ,   discreteLatticeEdges :: [DiscreteEdge]
    }
-- @-node:gcross.20100717003017.2424:DiscreteLattice
-- @+node:gcross.20100717003017.2425:DiscreteVertex
data DiscreteVertex = DiscreteVertex
    {   discreteVertexLocationX :: Int
    ,   discreteVertexLocationY :: Int
    ,   discreteVertexOrientation :: Int
    } deriving (Eq,Ord,Show)
-- @-node:gcross.20100717003017.2425:DiscreteVertex
-- @-node:gcross.20100717003017.2423:Types
-- @+node:gcross.20100717003017.2429:Functions
-- @+node:gcross.20100717003017.2437:discretizeLattice
discretizeLattice :: Lattice → DiscreteLattice
discretizeLattice lattice@Lattice{..} =
    let computeLocationMap :: (Vertex → ApproximateDouble) → Map ApproximateDouble Int
        computeLocationMap getValue =
            Map.fromList
            .
            flip zip [0..]
            .
            Set.toList
            .
            Set.map getValue
            $
            latticeVertices
        x_location_map = computeLocationMap vertexLocationX
        y_location_map = computeLocationMap vertexLocationY
        orientations_map = computeLocationMap vertexOrientation

        vertices = Set.toList latticeVertices

        discrete_vertices =
            map (\Vertex{..} →
                DiscreteVertex
                    (fromJust . Map.lookup vertexLocationX $ x_location_map)
                    (fromJust . Map.lookup vertexLocationX $ y_location_map)
                    (fromJust . Map.lookup vertexOrientation $ orientations_map)
            ) vertices

        vertex_number_map =
            Map.fromList
            .
            flip zip [0..]
            $
            vertices

        lookupVertexNumber :: Vertex → Int
        lookupVertexNumber =
            fromJust
            .
            flip Map.lookup vertex_number_map

        discretizeEdgeSide :: EdgeSide → DiscreteEdgeSide
        discretizeEdgeSide EdgeSide{..} =
            DiscreteEdgeSide
                (lookupVertexNumber edgeSideVertex)
                edgeSideRayNumber

        discrete_edges =
            map (liftA2 DiscreteEdge
                    (discretizeEdgeSide . edgeLeftSide)
                    (discretizeEdgeSide . edgeRightSide)
            ) latticeEdges

    in DiscreteLattice
        {   discreteLatticeVertices = Seq.fromList discrete_vertices
        ,   discreteLatticeEdges = discrete_edges
        }
-- @-node:gcross.20100717003017.2437:discretizeLattice
-- @+node:gcross.20100717003017.2431:drawDiscreteLattice
drawDiscreteLattice :: DiscreteLattice → String
drawDiscreteLattice DiscreteLattice{discreteLatticeVertices}
  | Seq.null discreteLatticeVertices = ""
  | otherwise =
    let coordinate_map = 
            Map.fromList
            .
            map ((discreteVertexLocationX &&& discreteVertexLocationY) &&&
                 discreteVertexOrientation
                )
            .
            toList
            $
            discreteLatticeVertices
        minmax getCoordinate =
            (minimum &&& maximum)
            .
            map getCoordinate
            .
            Map.keys
            $
            coordinate_map
        (min_X,max_X) = minmax fst
        (min_Y,max_Y) = minmax snd
    in  unlines
        .
        transpose
        .
        removeBlankLines
        .
        transpose
        .
        removeBlankLines
        $
        [[maybe ' ' (chr . (+ ord '0')) (Map.lookup (x,y) coordinate_map)
         | x ← [min_X..max_X]
         ]
        | y ← [max_Y,max_Y-1..min_Y]
        ]
  where
    removeBlankLines = filter (any (/= ' '))
-- @nonl
-- @-node:gcross.20100717003017.2431:drawDiscreteLattice
-- @+node:gcross.20100717003017.2433:getAndDrawLattice
getAndDrawLattice :: LatticeMonad String
getAndDrawLattice =
    fmap (
        drawDiscreteLattice
        .
        discretizeLattice
    ) getLattice
-- @-node:gcross.20100717003017.2433:getAndDrawLattice
-- @+node:gcross.20100717003017.2435:getAndDrawPrunedLattice
getAndDrawPrunedLattice :: LatticeMonad String
getAndDrawPrunedLattice =
    fmap (
        drawDiscreteLattice
        .
        discretizeLattice
        .
        pruneLattice
    ) getLattice
-- @-node:gcross.20100717003017.2435:getAndDrawPrunedLattice
-- @+node:gcross.20100717003017.2439:numberOfEdgesInDiscreteLattice
numberOfEdgesInDiscreteLattice =
    length
    .
    discreteLatticeEdges
-- @-node:gcross.20100717003017.2439:numberOfEdgesInDiscreteLattice
-- @+node:gcross.20100717003017.2441:numberOfOrientationsInDiscreteLattice
numberOfOrientationsInDiscreteLattice =
    (+1)
    .
    maximum
    .
    map discreteVertexOrientation
    .
    toList
    .
    discreteLatticeVertices
-- @-node:gcross.20100717003017.2441:numberOfOrientationsInDiscreteLattice
-- @+node:gcross.20100717003017.2443:numberOfRaysInDiscreteLattice
numberOfRaysInDiscreteLattice =
    (+1)
    .
    maximum
    .
    map (
        uncurry (max `on` discreteEdgeSideRayNumber)
        .
        (discreteEdgeLeftSide &&& discreteEdgeRightSide)
    )
    .
    toList
    .
    discreteLatticeEdges
-- @-node:gcross.20100717003017.2443:numberOfRaysInDiscreteLattice
-- @+node:gcross.20100717003017.2438:numberOfVerticesInDiscreteLattice
numberOfVerticesInDiscreteLattice =
    Seq.length
    .
    discreteLatticeVertices
-- @-node:gcross.20100717003017.2438:numberOfVerticesInDiscreteLattice
-- @-node:gcross.20100717003017.2429:Functions
-- @-others
-- @-node:gcross.20100717003017.2420:@thin Discrete.hs
-- @-leo

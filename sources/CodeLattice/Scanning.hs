-- @+leo-ver=4-thin
-- @+node:gcross.20100314233604.1666:@thin Scanning.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100315120315.1445:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100315120315.1445:<< Language extensions >>
-- @nl

module CodeLattice.Scanning where

-- @<< Import needed modules >>
-- @+node:gcross.20100314233604.1667:<< Import needed modules >>
import Control.Arrow
import Control.Applicative
import Control.Monad

import Data.Array.Storable
import qualified Data.Bimap as Bimap
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Language.Haskell.TH

import System.IO.Unsafe

import CodeLattice
import CodeLattice.Discrete
import CodeLattice.Labeling

import Debug.Trace
-- @nonl
-- @-node:gcross.20100314233604.1667:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100314233604.1668:Types
-- @+node:gcross.20100314233604.1669:ScanConfiguration
data ScanConfiguration = ScanConfiguration
    {   scanNumberOfQubits :: CInt
    ,   scanNumberOfOperators :: CInt
    ,   scanOperatorTable :: StorableArray (Int,Int) CInt
    }
-- @-node:gcross.20100314233604.1669:ScanConfiguration
-- @+node:gcross.20100714222047.1668:Solution
data Solution = Solution
    {   solutionNumberOfStabilizers :: CInt
    ,   solutionNumberOfGaugeQubits :: CInt
    ,   solutionNumberOfLogicalQubits :: CInt
    ,   solutionLogicalQubitDistances :: [CInt]
    } deriving (Eq,Show)
-- @-node:gcross.20100714222047.1668:Solution
-- @-node:gcross.20100314233604.1668:Types
-- @+node:gcross.20100728132013.1641:Solvers
foreign import ccall solve_any ::
    CInt → CInt →
    Ptr CInt → Ptr CInt →
    Bool →
    Ptr CInt → Ptr CInt →
    Ptr CInt → Ptr (Ptr CInt) →
    IO ()

$(let
    sizes = [4,6,8,12,16,18,24,32,36,48,54,64,72,96,100,108,128,144,150,162,192,200,216,256,288,300,384,400,432,450,576,600,648,864]
    solverName = ("solve_" ++) . show
    solverFn = mkName . solverName
    sizes_and_solvers_list =
        return .  ListE $
        [ TupE [LitE (IntegerL size),VarE (solverFn size)]
        | size ← sizes
        ]
  in do
    solver_type ← [t|CInt → CInt → Ptr CInt → Ptr CInt → Bool → Ptr CInt → Ptr CInt → Ptr CInt → Ptr (Ptr CInt) → IO ()|]
    solver_map_declaration ← [d| solvers = IntMap.fromList $(sizes_and_solvers_list) |]
    solver_foreign_import_declarations ← return
        [ForeignD $
            ImportF
                CCall
                Safe
                (solverName size)
                (solverFn size)
                solver_type
        | size ← sizes
        ]
    return (solver_map_declaration ++ solver_foreign_import_declarations)
 )

getSolver = fromMaybe solve_any . flip IntMap.lookup solvers . fromIntegral
-- @-node:gcross.20100728132013.1641:Solvers
-- @+node:gcross.20100315191926.2795:Functions
-- @+node:gcross.20100728132013.1993:collectDistances
collectDistances :: [CInt] → [(CInt,Int)]
collectDistances [] = []
collectDistances (x:xs) = go x 1 xs
  where
    go current_value current_count [] = [(current_value,current_count)]
    go current_value current_count (next_value:rest_values)
      | current_value == next_value
        = go current_value (current_count+1) rest_values
      | otherwise
        = (current_value,current_count):go next_value 1 rest_values
-- @-node:gcross.20100728132013.1993:collectDistances
-- @+node:gcross.20100315191926.2799:solve(Noisily)ForLabeling
solveForLabelingWithVerbosity :: Bool → ScanConfiguration → LatticeLabeling → IO Solution
solveForLabelingWithVerbosity verbosity ScanConfiguration{..} labeling =
    withStorableArray scanOperatorTable $ \p_operator_table →
    withArray ((map fromIntegral . flattenLatticeLabeling) labeling) $ \p_values →
    alloca $ \p_number_of_stabilizers →
    alloca $ \p_number_of_gauge_qubits →
    alloca $ \p_number_of_logical_qubits →
    alloca $ \p_p_logical_qubit_distances → do
        getSolver scanNumberOfQubits
            scanNumberOfQubits scanNumberOfOperators
            p_operator_table p_values
            verbosity
            p_number_of_stabilizers p_number_of_gauge_qubits
            p_number_of_logical_qubits p_p_logical_qubit_distances
        number_of_logical_qubits ← peek p_number_of_logical_qubits
        p_logical_qubit_distances ← peek p_p_logical_qubit_distances
        solution ←
            Solution
            <$> (peek p_number_of_stabilizers)
            <*> (peek p_number_of_gauge_qubits)
            <*> (pure number_of_logical_qubits)
            <*> (peekArray
                    (fromIntegral number_of_logical_qubits)
                    p_logical_qubit_distances
                )
        free p_logical_qubit_distances
        return solution

solveForLabeling :: ScanConfiguration → LatticeLabeling → Solution
solveForLabeling config labeling =
    unsafePerformIO
    $
    solveForLabelingWithVerbosity False config labeling

solveForLabelingNoisily :: ScanConfiguration → LatticeLabeling → IO Solution
solveForLabelingNoisily = solveForLabelingWithVerbosity True
-- @-node:gcross.20100315191926.2799:solve(Noisily)ForLabeling
-- @+node:gcross.20100314233604.1671:latticeToScanConfiguration
latticeToScanConfiguration :: DiscreteLattice → ScanConfiguration
latticeToScanConfiguration lattice@DiscreteLattice{..} =
    ScanConfiguration
    {   scanNumberOfQubits = fromIntegral number_of_vertices
    ,   scanNumberOfOperators = fromIntegral number_of_edges
    ,   scanOperatorTable = operator_table
    }
  where
    number_of_vertices = numberOfVerticesInDiscreteLattice lattice
    number_of_orientations = numberOfOrientationsInDiscreteLattice lattice
    number_of_edges = numberOfEdgesInDiscreteLattice lattice

    computeLabelIndex vertex_number ray_number =
        ray_number * number_of_orientations
        +
        (   discreteVertexOrientation
            .
            Seq.index discreteLatticeVertices
            $
            vertex_number
        )

    operator_table =
        unsafePerformIO
        .
        newListArray ((0,0),(number_of_edges-1,4-1))
        .
        map fromIntegral
        .
        concat
        .
        map (\(DiscreteEdge
                (DiscreteEdgeSide vertex_number_1 ray_number_1)
                (DiscreteEdgeSide vertex_number_2 ray_number_2)
              ) →
                [vertex_number_1
                ,computeLabelIndex vertex_number_1 ray_number_1
                ,vertex_number_2
                ,computeLabelIndex vertex_number_2 ray_number_2
                ]
        )
        $
        discreteLatticeEdges
-- @-node:gcross.20100314233604.1671:latticeToScanConfiguration
-- @-node:gcross.20100315191926.2795:Functions
-- @-others
-- @-node:gcross.20100314233604.1666:@thin Scanning.hs
-- @-leo

-- @+leo-ver=4-thin
-- @+node:gcross.20100314233604.1666:@thin Scanning.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100315120315.1445:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100315120315.1445:<< Language extensions >>
-- @nl

-- @<< Link dependencies >>
-- @+node:gcross.20100315191926.1987:<< Link dependencies >>
{-# BLUEPRINT-LINK-DEPENDENCY CodeLattice.Scanning.C o #-}
-- @-node:gcross.20100315191926.1987:<< Link dependencies >>
-- @nl

module CodeLattice.Scanning where

-- @<< Import needed modules >>
-- @+node:gcross.20100314233604.1667:<< Import needed modules >>
import Control.Arrow
import Control.Applicative
import Control.Monad

import qualified Data.Bimap as Bimap
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import Data.NDArray hiding ((!))
import Data.NDArray.Classes
import Data.Vec ((:.)(..))
import qualified Data.Vec as V

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

import CodeLattice
-- @-node:gcross.20100314233604.1667:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100314233604.1668:Types
-- @+node:gcross.20100314233604.1669:ScanConfiguration
data ScanConfiguration = ScanConfiguration
    {   scanNumberOfQubits :: CInt
    ,   scanNumberOfOperators :: CInt
    ,   scanNumberOfOrientations :: CInt
    ,   scanNumberOfRays :: CInt
    ,   scanOperatorTable :: Array2D CInt
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
-- @+node:gcross.20100713173607.1613:VertexLabeling
newtype VertexLabeling = VertexLabeling { unwrapVertexLabeling :: [Int] } deriving (Eq,Ord,Show)


-- @-node:gcross.20100713173607.1613:VertexLabeling
-- @+node:gcross.20100713173607.1614:LatticeLabeling
newtype LatticeLabeling = LatticeLabeling { unwrapLatticeLabeling :: [VertexLabeling] } deriving (Eq,Ord,Show)
-- @nonl
-- @-node:gcross.20100713173607.1614:LatticeLabeling
-- @-node:gcross.20100314233604.1668:Types
-- @+node:gcross.20100315191926.2795:C Functions
-- @+node:gcross.20100315191926.2799:solve(Noisily)ForLabeling
-- @+at
--  extern "C" void solve(
--      int number_of_qubits, int number_of_operators,
--      int* restrict operator_table, int* restrict values,
--      bool noisy,
--      int* restrict number_of_stabilizers, int* restrict 
--  number_of_gauge_qubits,
--      int* restrict number_of_logical_qubits, int ** restrict 
--  logical_qubit_distances
--  )
-- @-at
-- @@c

foreign import ccall solve ::
    CInt → CInt →
    Ptr CInt → Ptr CInt →
    Bool →
    Ptr CInt → Ptr CInt →
    Ptr CInt → Ptr (Ptr CInt) →
    IO CInt

solveForLabelingWithVerbosity :: Bool → ScanConfiguration → LatticeLabeling → IO Solution
solveForLabelingWithVerbosity verbosity config labeling =
    withNDArray (scanOperatorTable config) $ \p_operator_table →
    withArray ((map fromIntegral . flattenLatticeLabeling) labeling) $ \p_values →
    alloca $ \p_number_of_stabilizers →
    alloca $ \p_number_of_gauge_qubits →
    alloca $ \p_number_of_logical_qubits →
    alloca $ \p_p_logical_qubit_distances → do
        solve
            (scanNumberOfQubits config) (scanNumberOfOperators config)
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
-- @-node:gcross.20100315191926.2795:C Functions
-- @+node:gcross.20100314233604.1670:Functions
-- @+node:gcross.20100714222047.1669:flattenLatticeLabeling
flattenLatticeLabeling :: LatticeLabeling → [Int]
flattenLatticeLabeling =
    concat
    .
    transpose
    .
    map unwrapVertexLabeling
    .
    unwrapLatticeLabeling
-- @-node:gcross.20100714222047.1669:flattenLatticeLabeling
-- @+node:gcross.20100314233604.1671:latticeToScanConfiguration
latticeToScanConfiguration :: Int → Int → PositionSpaceLattice → ScanConfiguration
latticeToScanConfiguration number_of_orientations number_of_rays (PositionSpaceLattice (Lattice vertices edges)) =
    ScanConfiguration
    {   scanNumberOfQubits = fromIntegral number_of_vertices
    ,   scanNumberOfOperators = fromIntegral number_of_edges
    ,   scanNumberOfOrientations = fromIntegral number_of_orientations
    ,   scanNumberOfRays = fromIntegral number_of_rays
    ,   scanOperatorTable = operator_table
    }
  where
    number_of_vertices = Bimap.size vertices
    vertex_map =
        IntMap.fromAscList
        .
        map (\(qubit_number,(vertex_number,vertex)) →
                (vertex_number,(qubit_number,vertexOrientation vertex))
            )
        .
        zip [0..]
        .
        Bimap.toAscList
        $
        vertices
    number_of_edges = length edges
    operator_table =
        fromListWithShape (number_of_edges :. 4 :. ())
        .
        map fromIntegral
        .
        concat
        .
        map (\(Edge (EdgeSide vertex_number_1 ray_number_1)
                    (EdgeSide vertex_number_2 ray_number_2)
              ) →
                let (qubit_number_1, orientation_number_1) =
                        fromJust $ IntMap.lookup vertex_number_1 vertex_map
                    (qubit_number_2, orientation_number_2) =
                        fromJust $ IntMap.lookup vertex_number_2 vertex_map
                in [qubit_number_1
                   ,ray_number_1 * number_of_orientations + orientation_number_1
                   ,qubit_number_2
                   ,ray_number_2 * number_of_orientations + orientation_number_2
                   ]
        )
        $
        edges
-- @-node:gcross.20100314233604.1671:latticeToScanConfiguration
-- @+node:gcross.20100316133702.1465:computeNumberOfLabelings
computeNumberOfLabelings :: ScanConfiguration → Integer
computeNumberOfLabelings
    (ScanConfiguration
        {   scanNumberOfOrientations = number_of_orientations
        ,   scanNumberOfRays = number_of_rays
        }
    ) = (product $ genericReplicate number_of_orientations 2)
        *
        (product $ genericReplicate (number_of_orientations*(number_of_rays-2)) 3)
-- @nonl
-- @-node:gcross.20100316133702.1465:computeNumberOfLabelings
-- @+node:gcross.20100713003314.1568:canonicalizeVertexLabeling
canonicalizeVertexLabeling :: VertexLabeling → VertexLabeling
canonicalizeVertexLabeling (VertexLabeling old_labeling) =
    VertexLabeling
    .
    map ((+1) . fromJust . flip elemIndex (nub old_labeling))
    $
    old_labeling
-- @-node:gcross.20100713003314.1568:canonicalizeVertexLabeling
-- @+node:gcross.20100713115329.1573:generateVertexLabelings
generateVertexLabelings :: Int → [VertexLabeling]
generateVertexLabelings = map (VertexLabeling . (1:)) . go . (\x → x-1)
  where
    go 0 = [[]]
    go n =
        (map (1:) (go (n-1)))
        ++
        (map (2:) (replicateM (n-1) [1..3]))
-- @-node:gcross.20100713115329.1573:generateVertexLabelings
-- @+node:gcross.20100713115329.1584:generateLatticeLabelings
generateLatticeLabelings :: Int → Int → [LatticeLabeling]
generateLatticeLabelings number_of_vertices number_of_rays =
    map LatticeLabeling
    .
    replicateM number_of_vertices
    .
    generateVertexLabelings
    $
    number_of_rays
-- @nonl
-- @-node:gcross.20100713115329.1584:generateLatticeLabelings
-- @+node:gcross.20100713115329.1582:canonicalizeLatticeLabeling
canonicalizeLatticeLabeling :: LatticeLabeling → LatticeLabeling
canonicalizeLatticeLabeling = LatticeLabeling . map canonicalizeVertexLabeling . unwrapLatticeLabeling
-- @nonl
-- @-node:gcross.20100713115329.1582:canonicalizeLatticeLabeling
-- @+node:gcross.20100714141137.1609:permuteVertexLabeling
permuteVertexLabeling ::  VertexLabeling → VertexLabelingPermutation → VertexLabeling
permuteVertexLabeling (VertexLabeling old_labeling) =
    VertexLabeling
    .
    map (old_labeling !!)
    .
    unwrapVertexLabelingPermutation

-- @-node:gcross.20100714141137.1609:permuteVertexLabeling
-- @+node:gcross.20100714141137.1611:permuteLatticeLabeling
permuteLatticeLabeling ::  LatticeLabeling → LatticeLabelingPermutation → LatticeLabeling
permuteLatticeLabeling (LatticeLabeling old_labeling) =
    LatticeLabeling
    .
    map (uncurry (permuteVertexLabeling . (old_labeling !!)))
    .
    unwrapLatticeLabelingPermutation
-- @-node:gcross.20100714141137.1611:permuteLatticeLabeling
-- @-node:gcross.20100314233604.1670:Functions
-- @-others
-- @-node:gcross.20100314233604.1666:@thin Scanning.hs
-- @-leo

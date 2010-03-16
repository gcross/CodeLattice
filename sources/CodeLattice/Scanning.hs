-- @+leo-ver=4-thin
-- @+node:gcross.20100314233604.1666:@thin Scanning.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100315120315.1445:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
import Foreign.Marshal.Array
import Foreign.Ptr

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
-- @-node:gcross.20100314233604.1668:Types
-- @+node:gcross.20100315191926.2795:C Functions
-- @+node:gcross.20100315191926.2799:solve(Noisily)ForLabeling
foreign import ccall solve :: CInt -> CInt -> Ptr CInt -> Ptr CInt -> Bool -> IO CInt

solveForLabeling :: ScanConfiguration -> [CInt] -> CInt
solveForLabeling config values =
    unsafePerformIO $
    withNDArray (scanOperatorTable config) $ \p_operator_table ->
    withArray values $ \p_values ->
        solve
            (scanNumberOfQubits config)
            (scanNumberOfOperators config)
            p_operator_table
            p_values
            False

solveNoisilyForLabeling :: ScanConfiguration -> [CInt] -> IO CInt
solveNoisilyForLabeling config values =
    withNDArray (scanOperatorTable config) $ \p_operator_table ->
    withArray values $ \p_values ->
        solve
            (scanNumberOfQubits config)
            (scanNumberOfOperators config)
            p_operator_table
            p_values
            True
-- @-node:gcross.20100315191926.2799:solve(Noisily)ForLabeling
-- @-node:gcross.20100315191926.2795:C Functions
-- @+node:gcross.20100314233604.1670:Functions
-- @+node:gcross.20100314233604.1671:latticeToScanConfiguration
latticeToScanConfiguration :: Int -> Int -> Lattice -> ScanConfiguration
latticeToScanConfiguration number_of_orientations number_of_rays (Lattice vertices edges) =
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
        map (\(qubit_number,(vertex_number,vertex)) ->
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
              ) -> let (qubit_number_1, orientation_number_1) =
                           fromJust $ IntMap.lookup vertex_number_1 vertex_map
                       (qubit_number_2, orientation_number_2) =
                           fromJust $ IntMap.lookup vertex_number_2 vertex_map
                   in  [qubit_number_1
                       ,ray_number_1 * number_of_orientations + orientation_number_1
                       ,qubit_number_2
                       ,ray_number_2 * number_of_orientations + orientation_number_2
                       ]
        )
        $
        edges
-- @-node:gcross.20100314233604.1671:latticeToScanConfiguration
-- @+node:gcross.20100315120315.1425:scanOverLabelings
scanOverLabelings :: Monad m => ScanConfiguration -> ([CInt] -> m ()) -> m ()
{-# INLINE scanOverLabelings #-}
scanOverLabelings
    (ScanConfiguration
        {   scanNumberOfOrientations = number_of_orientations
        ,   scanNumberOfRays = number_of_rays
        }
    )
    thunk
    = runThunkOverChoices thunk $
        genericReplicate number_of_orientations [1]
        ++
        genericReplicate number_of_orientations [1,2]
        ++
        genericReplicate (number_of_orientations*(number_of_rays-2)) [1,2,3]
-- @-node:gcross.20100315120315.1425:scanOverLabelings
-- @+node:gcross.20100316133702.1467:runThunkOverChoices
runThunkOverChoices :: Monad m => ([a] -> m ()) -> [[a]] -> m ()
{-# INLINE runThunkOverChoices #-}
runThunkOverChoices thunk lists =
    go (reverse lists) []
  where
    go [] stack = thunk stack
    go (values:rest_lists) stack =
        mapM_ (go rest_lists . (:stack)) values
-- @-node:gcross.20100316133702.1467:runThunkOverChoices
-- @+node:gcross.20100316133702.1465:computeNumberOfLabelings
computeNumberOfLabelings :: ScanConfiguration -> Integer
computeNumberOfLabelings
    (ScanConfiguration
        {   scanNumberOfOrientations = number_of_orientations
        ,   scanNumberOfRays = number_of_rays
        }
    ) = (product $ genericReplicate number_of_orientations 2)
        *
        (product $ genericReplicate (number_of_orientations*(number_of_rays-2)) 3)
-- @-node:gcross.20100316133702.1465:computeNumberOfLabelings
-- @-node:gcross.20100314233604.1670:Functions
-- @-others
-- @-node:gcross.20100314233604.1666:@thin Scanning.hs
-- @-leo
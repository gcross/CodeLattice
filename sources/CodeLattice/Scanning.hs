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

import Data.Array
import Data.Array.MArray
import Data.Array.Storable
import qualified Data.Bimap as Bimap
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import Data.NDArray hiding ((!))
import Data.NDArray.Classes
import Data.Vec ((:.)(..))
import qualified Data.Vec as V

import Foreign.C
import Foreign.Ptr

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
-- @+node:gcross.20100315191926.2799:solveForLabeling
foreign import ccall solve :: CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

solveForLabeling :: ScanConfiguration -> StorableArray Int CInt -> IO CInt
solveForLabeling config values =
    withNDArray (scanOperatorTable config) $ \p_operator_table ->
    withStorableArray values $ \p_values ->
        solve
            (fromIntegral $ scanNumberOfQubits config)
            (fromIntegral $ scanNumberOfOperators config)
            p_operator_table
            p_values
-- @-node:gcross.20100315191926.2799:solveForLabeling
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
scanOverLabelings :: Monad m => ScanConfiguration -> ([(Int,CInt)] -> m ()) -> m ()
{-# INLINE scanOverLabelings #-}
scanOverLabelings
    (ScanConfiguration
        {   scanNumberOfOrientations = number_of_orientations
        ,   scanNumberOfRays = number_of_rays
        }
    )
    thunk
    = runThunkOverUpdates thunk $
        genericReplicate number_of_orientations [1]
        ++
        genericReplicate number_of_orientations [1,2]
        ++
        genericReplicate (number_of_orientations*(number_of_rays-2)) [1,2,3]
-- @-node:gcross.20100315120315.1425:scanOverLabelings
-- @+node:gcross.20100315120315.1426:runThunkOverUpdates
runThunkOverUpdates :: Monad m => ([(Int,a)] -> m ()) -> [[a]] -> m ()
{-# INLINE runThunkOverUpdates #-}
runThunkOverUpdates thunk lists =
    go 0 lists []
  where
    go _ [] stack = thunk stack
    go index ((value:rest_values):rest) stack =
        goNext ((index,value):stack)
        >>
        mapM_ (goNext . (:[]) . (,) index) rest_values
      where
        goNext = go (index+1) rest
-- @-node:gcross.20100315120315.1426:runThunkOverUpdates
-- @+node:gcross.20100315191926.1435:applyUpdates
applyUpdates :: StorableArray Int CInt -> [(Int,CInt)] -> IO ()
applyUpdates = mapM_ . uncurry . writeArray
-- @-node:gcross.20100315191926.1435:applyUpdates
-- @+node:gcross.20100316133702.1464:newLabelingArray
newLabelingArray :: ScanConfiguration -> IO (StorableArray Int CInt)
newLabelingArray
    (ScanConfiguration
        {   scanNumberOfOrientations = number_of_orientations
        ,   scanNumberOfRays = number_of_rays
        }
    ) = newArray_ (0,fromIntegral $ number_of_orientations*number_of_rays-1)
-- @-node:gcross.20100316133702.1464:newLabelingArray
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

-- @+leo-ver=4-thin
-- @+node:gcross.20100314233604.1666:@thin Scanning.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100315120315.1445:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100315120315.1445:<< Language extensions >>
-- @nl

module CodeLattice.Scanning where

-- @<< Import needed modules >>
-- @+node:gcross.20100314233604.1667:<< Import needed modules >>
import Control.Monad

import Data.Array
import qualified Data.Bimap as Bimap
import Data.NDArray hiding ((!))
import Data.Vec ((:.)(..))
import qualified Data.Vec as V

import CodeLattice
-- @-node:gcross.20100314233604.1667:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100314233604.1668:Types
-- @+node:gcross.20100314233604.1669:ScanConfiguration
data ScanConfiguration = ScanConfiguration
    {   scanNumberOfQubits :: Int
    ,   scanNumberOfOperators :: Int
    ,   scanNumberOfOrientations :: Int
    ,   scanNumberOfRays :: Int
    ,   scanOperatorTable :: Array2D Int
    }
-- @-node:gcross.20100314233604.1669:ScanConfiguration
-- @-node:gcross.20100314233604.1668:Types
-- @+node:gcross.20100314233604.1670:Functions
-- @+node:gcross.20100314233604.1671:latticeToScanConfiguration
latticeToScanConfiguration :: Int -> Int -> Lattice -> ScanConfiguration
latticeToScanConfiguration number_of_orientations number_of_rays (Lattice vertices edges) =
    ScanConfiguration
    {   scanNumberOfQubits = number_of_vertices
    ,   scanNumberOfOperators = number_of_edges
    ,   scanNumberOfOrientations = number_of_orientations
    ,   scanNumberOfRays = number_of_rays
    ,   scanOperatorTable = operator_table
    }
  where
    number_of_vertices = Bimap.size vertices
    vertex_orientation_map =
        listArray (0,number_of_vertices)
        .
        map (vertexOrientation . snd)
        .
        Bimap.toAscList
        $
        vertices
    number_of_edges = length edges
    operator_table =
        fromListWithShape (number_of_edges :. 2 :. ())
        .
        concat
        .
        map (\(Edge (EdgeSide vertex_number_1 ray_number_1)
                    (EdgeSide vertex_number_2 ray_number_2)
              ) -> [vertex_number_1
                   ,ray_number_1 * number_of_orientations + (vertex_orientation_map ! vertex_number_1)
                   ,vertex_number_2
                   ,ray_number_2 * number_of_orientations + (vertex_orientation_map ! vertex_number_2)
                   ]
        )
        $
        edges
-- @-node:gcross.20100314233604.1671:latticeToScanConfiguration
-- @+node:gcross.20100315120315.1425:runThunkOverLabelingUpdates
runThunkOverLabelingUpdates :: Monad m => ([(Int,Int)] -> m ()) -> Int -> Int -> m ()
{-# INLINE runThunkOverLabelingUpdates #-}
runThunkOverLabelingUpdates thunk number_of_orientations number_of_rays =
    runThunkOverUpdates thunk $
        replicate number_of_orientations [1,3]
        ++
        replicate (number_of_orientations*(number_of_rays-1)) [1,2,3]
-- @-node:gcross.20100315120315.1425:runThunkOverLabelingUpdates
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
-- @-node:gcross.20100314233604.1670:Functions
-- @-others
-- @-node:gcross.20100314233604.1666:@thin Scanning.hs
-- @-leo

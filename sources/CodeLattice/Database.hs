-- @+leo-ver=4-thin
-- @+node:gcross.20100312175547.1384:@thin Database.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100312175547.1842:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100312175547.1842:<< Language extensions >>
-- @nl

module CodeLattice.Database where

-- @<< Import needed modules >>
-- @+node:gcross.20100312175547.1385:<< Import needed modules >>
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import qualified Data.Bimap as Bimap
import Data.ConfigFile
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.UUID

import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Database.PostgreSQL.PGFunctions

import System.Exit
import System.IO.Unsafe
import System.Random

import CodeLattice
-- @-node:gcross.20100312175547.1385:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100312175547.1823:Enumerators
-- @+node:gcross.20100312175547.1824:getX
get1 :: (Monad m) => a -> IterAct m (Maybe a)
get1 x _ = return $ Left $ Just $! x

get2 :: (Monad m) => a -> b -> IterAct m (Maybe (a,b))
get2 x y _ = return $ Left $ Just $! (x,y)
-- @-node:gcross.20100312175547.1824:getX
-- @+node:gcross.20100312175547.1825:fetchX
fetch1 :: (Monad m) => a -> IterAct m [a]
fetch1 a accum = result' (a:accum) --'

fetch2 :: (Monad m) => a -> b -> IterAct m [(a, b)]
fetch2 a b accum = result' ((a, b):accum) --'

fetch3 :: (Monad m) => a -> b -> c -> IterAct m [(a, b, c)]
fetch3 a b c accum = result' ((a, b, c):accum) --'

fetch4 :: (Monad m) => a -> b -> c -> d -> IterAct m [(a, b, c, d)]
fetch4 a b c d accum = result' ((a, b, c, d):accum) --'
-- @-node:gcross.20100312175547.1825:fetchX
-- @-node:gcross.20100312175547.1823:Enumerators
-- @+node:gcross.20100312175547.1817:Functions
-- @+node:gcross.20100312220352.1834:edgeIteratee
edgeIteratee :: (MonadIO m) => Int -> Int -> Int -> Int -> IterAct m [Edge]
edgeIteratee
    vertex_number_1 ray_number_1
    vertex_number_2 ray_number_2
    edges
    =
    result' (Edge (EdgeSide vertex_number_1 ray_number_1) -- '
                  (EdgeSide vertex_number_2 ray_number_2)
            :edges
            )
-- @-node:gcross.20100312220352.1834:edgeIteratee
-- @+node:gcross.20100312220352.1836:vertexIteratee
vertexIteratee :: (MonadIO m) => Int -> Int -> Int -> Int -> IterAct m [(Int,Vertex)]
vertexIteratee vertex_number x y orientation vertices =
    result' ((vertex_number -- '
             ,Vertex (Location x y) orientation
             )
            :vertices
            )
-- @-node:gcross.20100312220352.1836:vertexIteratee
-- @+node:gcross.20100312175547.1819:makeConnection
makeConnection heading = do
    either_conn <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP "connection.cfg"
        host <- get cp "data source" "host"
        database <- get cp "data source" "database"
        user <- get cp heading "user"
        password <- get cp heading "password"
        return $ connect
            [   CAhost host
            ,   CAdbname database
            ,   CAuser user
            ,   CApassword password
            ]
    case either_conn of
        Left err -> do
            print err
            exitFailure
        Right conn -> return conn
-- @-node:gcross.20100312175547.1819:makeConnection
-- @+node:gcross.20100312175547.1827:generateRandomUUIDAsString
generateRandomUUIDAsString :: (MonadIO m) => m String
generateRandomUUIDAsString = liftIO (fmap show (randomIO :: IO UUID))

-- @-node:gcross.20100312175547.1827:generateRandomUUIDAsString
-- @+node:gcross.20100312175547.1837:insertRow
insertRow name statement type_ids = insertRows name statement type_ids . (:[])
-- @-node:gcross.20100312175547.1837:insertRow
-- @+node:gcross.20100312175547.1833:insertRows
insertRows name statement type_ids rows =
    withPreparedStatement
        (prepareCommand name (sql statement) type_ids)
        (\prepared_statement ->
            (forM rows $
                \row -> withBoundStatement prepared_statement row $
                    \bound_statement -> execDML bound_statement
            )
            >>=
            (return . sum)
        )
    >>=
    \number_of_rows_inserted ->
        unless (number_of_rows_inserted == length rows) . error $
            "Inserted "
            ++ show number_of_rows_inserted ++
            " rows, but had been given "
            ++ show (length rows) ++
            " rows."
-- @-node:gcross.20100312175547.1833:insertRows
-- @+node:gcross.20100312175547.1831:storeLattice
storeLattice ::
    String ->
    Int ->
    Lattice ->
    (forall mark . DBM mark Session String)
storeLattice tiling_name growth_iteration_number (Lattice vertices edges) =
    generateRandomUUIDAsString
    >>=
    \lattice_id -> do
        insertRow
            "insert_lattice"
            "insert into lattices (lattice_id, tiling_name, growth_iteration_number, number_of_vertices, number_of_edges) values ((?::uuid),?,?,?,?)"
            [pgTypeOid (undefined :: String)
            ,pgTypeOid (undefined :: String)
            ,pgTypeOid (undefined :: Int)
            ,pgTypeOid (undefined :: Int)
            ,pgTypeOid (undefined :: Int)
            ]
            [bindP $ lattice_id
            ,bindP $ tiling_name
            ,bindP $ growth_iteration_number
            ,bindP $ (Bimap.size vertices)
            ,bindP $ (length edges)
            ]

        insertRows
            "insert_vertices"
            "insert into vertices (lattice_id, vertex_number, x, y, orientation) values ((?::uuid),?,?,?,?)"
            [pgTypeOid (undefined :: String)
            ,pgTypeOid (undefined :: Int)
            ,pgTypeOid (undefined :: Int)
            ,pgTypeOid (undefined :: Int)
            ,pgTypeOid (undefined :: Int)
            ]
            [   [bindP $ lattice_id
                ,bindP $ vertex_number
                ,bindP $ x
                ,bindP $ y
                ,bindP $ orientation
                ]
            |   (vertex_number,Vertex (Location x y) orientation) <-
                    Bimap.toList vertices
            ]

        insertRows
            "insert_edges"
            "insert into edges (lattice_id, vertex_number_1, ray_number_1, vertex_number_2, ray_number_2) values ((?::uuid),?,?,?,?)"
            [pgTypeOid (undefined :: String)
            ,pgTypeOid (undefined :: Int)
            ,pgTypeOid (undefined :: Int)
            ,pgTypeOid (undefined :: Int)
            ,pgTypeOid (undefined :: Int)
            ]
            [   [bindP $ lattice_id
                ,bindP $ vertex_number_1
                ,bindP $ ray_1
                ,bindP $ vertex_number_2
                ,bindP $ ray_2
                ]
            |   Edge (EdgeSide vertex_number_1 ray_1) (EdgeSide vertex_number_2 ray_2)
                    <- edges
            ]
        return lattice_id
-- @-node:gcross.20100312175547.1831:storeLattice
-- @+node:gcross.20100312220352.1837:fetchLattice
fetchLattice lattice_id =
    liftM2 Lattice
    (   fmap Bimap.fromList $
        doQuery
            (sql $ "select vertex_number, x, y, orientation from vertices where lattice_id = '" ++ lattice_id ++ "'")
            vertexIteratee
            []
    )
    (   doQuery
            (sql $ "select vertex_number_1, ray_number_1, vertex_number_2, ray_number_2 from edges where lattice_id = '" ++ lattice_id ++ "'")
            edgeIteratee
            []
    )
-- @-node:gcross.20100312220352.1837:fetchLattice
-- @-node:gcross.20100312175547.1817:Functions
-- @-others
-- @-node:gcross.20100312175547.1384:@thin Database.hs
-- @-leo

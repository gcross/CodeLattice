-- @+leo-ver=4-thin
-- @+node:gcross.20100312175547.1384:@thin Database.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100312175547.1842:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100312175547.1842:<< Language extensions >>
-- @nl

module CodeLattice.Database where

-- @<< Import needed modules >>
-- @+node:gcross.20100312175547.1385:<< Import needed modules >>
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.ConfigFile
import qualified Data.Foldable as Fold
import Data.Maybe
import Data.Sequence (Seq,(|>))
import qualified Data.Sequence as Seq
import Data.UUID

import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Database.PostgreSQL.PGFunctions

import System.Exit
import System.IO.Unsafe
import System.Random

import Text.Printf

import CodeLattice.Tilings
import CodeLattice.Scanning
-- @-node:gcross.20100312175547.1385:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100312175547.1823:Enumerators
-- @+node:gcross.20100312175547.1824:getX
get1 :: (Monad m) ⇒ a → IterAct m (Maybe a)
get1 x _ = return $ Left $ Just $! x

get2 :: (Monad m) ⇒ a → b → IterAct m (Maybe (a,b))
get2 x y _ = return $ Left $ Just $! (x,y)

get3 :: (Monad m) ⇒ a → b → c → IterAct m (Maybe (a,b,c))
get3 x y z _ = return $ Left $ Just $! (x,y,z)

get4 :: (Monad m) ⇒ a → b → c → d → IterAct m (Maybe (a,b,c,d))
get4 x y z w _ = return $ Left $ Just $! (x,y,z,w)
-- @nonl
-- @-node:gcross.20100312175547.1824:getX
-- @+node:gcross.20100312175547.1825:fetchX
fetch1 :: (Monad m) ⇒ a → IterAct m [a]
fetch1 a accum = result' (a:accum) --'

fetch2 :: (Monad m) ⇒ a → b → IterAct m [(a, b)]
fetch2 a b accum = result' ((a, b):accum) --'

fetch3 :: (Monad m) ⇒ a → b → c → IterAct m [(a, b, c)]
fetch3 a b c accum = result' ((a, b, c):accum) --'

fetch4 :: (Monad m) ⇒ a → b → c → d → IterAct m [(a, b, c, d)]
fetch4 a b c d accum = result' ((a, b, c, d):accum) --'
-- @nonl
-- @-node:gcross.20100312175547.1825:fetchX
-- @-node:gcross.20100312175547.1823:Enumerators
-- @+node:gcross.20100728132013.1988:sql wrappers
-- @+node:gcross.20100728132013.1989:query
query stmt accum init message = 
    catchDB (
            doQuery stmt accum init
          ) (reportRethrowMsg $ message ++ "\n")
-- @-node:gcross.20100728132013.1989:query
-- @+node:gcross.20100728132013.1990:modify
modify stmt message = catchDB (
        execDML stmt
    ) (reportRethrowMsg $ message ++ "\n")
-- @nonl
-- @-node:gcross.20100728132013.1990:modify
-- @-node:gcross.20100728132013.1988:sql wrappers
-- @+node:gcross.20100312175547.1817:Functions
-- @+node:gcross.20100312175547.1819:makeConnection
makeConnection heading = do
    either_conn ← runErrorT $ do
        cp ← join $ liftIO $ readfile emptyCP "connection.cfg"
        host ← get cp "data source" "host"
        database ← get cp "data source" "database"
        user ← get cp heading "user"
        password ← get cp heading "password"
        return $ connect
            [   CAhost host
            ,   CAdbname database
            ,   CAuser user
            ,   CApassword password
            ]
    case either_conn of
        Left err → do
            print err
            exitFailure
        Right conn → return conn
-- @nonl
-- @-node:gcross.20100312175547.1819:makeConnection
-- @+node:gcross.20100312175547.1827:generateRandomUUIDAsString
generateRandomUUIDAsString :: (MonadIO m) ⇒ m String
generateRandomUUIDAsString = liftIO (fmap show (randomIO :: IO UUID))
-- @-node:gcross.20100312175547.1827:generateRandomUUIDAsString
-- @+node:gcross.20100312175547.1837:insertRow
insertRow name statement type_ids = insertRows name statement type_ids . (:[])
-- @-node:gcross.20100312175547.1837:insertRow
-- @+node:gcross.20100312175547.1833:insertRows
insertRows name statement type_ids rows =
    withPreparedStatement
        (prepareCommand name (sql statement) type_ids)
        (\prepared_statement →
            (forM rows $
                \row → withBoundStatement prepared_statement row $
                    \bound_statement → execDML bound_statement
            )
            >>=
            (return . sum)
        )
    >>=
    \number_of_rows_inserted →
        unless (number_of_rows_inserted == length rows) . error $
            "Inserted "
            ++ show number_of_rows_inserted ++
            " rows, but had been given "
            ++ show (length rows) ++
            " rows."
-- @nonl
-- @-node:gcross.20100312175547.1833:insertRows
-- @+node:gcross.20100728132013.1642:storeSolution
storeSolution :: String → Int → Integer → Solution → (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session ())
storeSolution tiling_name radius labeling_number Solution{..} = do
    modify (cmdbind
        "insert into codes (tiling,radius,labeling,number_of_stabilizers,number_of_gauge_qubits,number_of_logical_qubits) values (?,?,?::bigint,?::integer,?::integer,?::integer)"
        [bindP tiling_name
        ,bindP radius
        ,bindP labeling_number
        ,bindP solutionNumberOfStabilizers
        ,bindP solutionNumberOfGaugeQubits
        ,bindP solutionNumberOfLogicalQubits
        ]) "Error writing code information into codes table:"
    let collected_distances = collectDistances solutionLogicalQubitDistances
    forM_ collected_distances $ \(distance,number_of_qubits) →
        modify (cmdbind
            "insert into distances (tiling,radius,labeling,distance,number_of_qubits) values (?,?,?::bigint,?::integer,?)"
            [bindP tiling_name
            ,bindP radius
            ,bindP labeling_number
            ,bindP distance
            ,bindP number_of_qubits
            ]) "Error writing distance information into distance table:"
-- @-node:gcross.20100728132013.1642:storeSolution
-- @+node:gcross.20100728132013.1994:checkIfScanned
checkIfScanned :: String → Int → (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session Bool)
checkIfScanned tiling_name radius =
    fmap isJust $ query
        (sql $ printf "select 1 from scanned where tiling = '%s' and radius = %i" tiling_name radius)
        get1 (Nothing :: Maybe Int)
        "Error fetching record from the scanned table:"
-- @-node:gcross.20100728132013.1994:checkIfScanned
-- @+node:gcross.20100728211848.1644:checkIfSkipperScanned
checkIfSkipperScanned :: String → Int → Integer → Integer → (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session Bool)
checkIfSkipperScanned tiling_name radius skip offset =
    fmap isJust $ query
        (sql $ printf "select 1 from skipper_scanned where tiling = '%s' and radius = %i and skip = %i and \"offset\" = %i;" tiling_name radius skip offset)
        get1 (Nothing :: Maybe Int)
        "Error fetching record from the skipper scanned table:"
-- @-node:gcross.20100728211848.1644:checkIfSkipperScanned
-- @+node:gcross.20100728132013.1996:markAsScanned
markAsScanned :: String → Int → (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session ())
markAsScanned tiling_name radius =
    modify (sql $ printf "delete from checkpoints where tiling = '%s' and radius = %i and skip = 1 and \"offset\" = 0" tiling_name radius) "Error deleting checkpoints:"
    >>
    modify (sql $ printf "insert into scanned (tiling,radius) values ('%s',%i)" tiling_name radius) "Error marking lattice as having been completely scanned:"
    >>=
    \number_of_rows_inserted →
        unless (number_of_rows_inserted == 1) . error $
            printf "Wrong number of rows was inserted! (%i /= 1)" number_of_rows_inserted
-- @-node:gcross.20100728132013.1996:markAsScanned
-- @+node:gcross.20100728211848.1650:markAsSkipperScanned
markAsSkipperScanned :: String → Int → Integer → Integer → (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session ())
markAsSkipperScanned tiling_name radius skip offset = do
    modify (sql $ printf "delete from checkpoints where tiling = '%s' and radius = %i and skip = %i and \"offset\" = %i" tiling_name radius skip offset) "Error marking lattice as having been completely scanned:"
    modify (sql $ printf "insert into skipper_scanned (tiling,radius,skip,\"offset\") values ('%s',%i,%i,%i)" tiling_name radius skip offset) "Error marking lattice as having been completely scanned:"
        >>= 
        \number_of_rows_inserted →
            unless (number_of_rows_inserted == 1) . error $
                printf "Wrong number of rows was inserted! (%i /= 1)" number_of_rows_inserted
    number_of_skipper_scans_completed ← fmap fromJust $
        query
            (sql $ printf "select count(1) from skipper_scanned where tiling = '%s' and radius = %i and skip = %i;" tiling_name radius skip)
            get1 (Nothing :: Maybe Integer)
            "Error fetching count from the skipper scanned table:"
    when (number_of_skipper_scans_completed >= skip) $
        markAsScanned tiling_name radius
-- @-node:gcross.20100728211848.1650:markAsSkipperScanned
-- @+node:gcross.20100728211848.1645:fetchCheckpoint
fetchCheckpoint :: String → Int → Integer → Integer → (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session (Maybe Integer))
fetchCheckpoint tiling_name radius skip offset =
    query
        (sql $ printf "select next_labeling from checkpoints where tiling = '%s' and radius = %i and skip = %i and \"offset\" = %i;" tiling_name radius skip offset)
        get1 (Nothing :: Maybe Integer)
        "Error fetching record from the checkpoint table:"
-- @-node:gcross.20100728211848.1645:fetchCheckpoint
-- @+node:gcross.20100728211848.1647:fetchLatticeSkip
fetchLatticeSkip :: String → Int → (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session (Maybe Integer))
fetchLatticeSkip tiling_name radius =
    liftM2 (<|>)
        (query
            (sql $ printf "select skip from checkpoints where tiling = '%s' and radius = %i;" tiling_name radius)
            get1 (Nothing :: Maybe Integer)
            "Error fetching record from the checkpoint table:"
        )
        (query
            (sql $ printf "select skip from skipper_scanned where tiling = '%s' and radius = %i;" tiling_name radius)
            get1 (Nothing :: Maybe Integer)
            "Error fetching record from the skipper_scanned table:"
        )
-- @-node:gcross.20100728211848.1647:fetchLatticeSkip
-- @+node:gcross.20100728211848.1651:setCheckpoint
setCheckpoint :: String → Int → Integer → Integer → Integer → (forall mark. DBM mark Database.PostgreSQL.Enumerator.Session ())
setCheckpoint tiling_name radius skip offset next_labeling =
    modify (sql $ printf "update checkpoints set next_labeling = %i where tiling = '%s' and radius = %i and skip = %i and \"offset\" = %i;" next_labeling tiling_name radius skip offset) "Error updating a checkpoint:"
    >>=
    (\number_of_rows_updated →
        if (number_of_rows_updated > 0)
            then return number_of_rows_updated 
            else modify (sql $ printf "insert into checkpoints (tiling,radius,skip,\"offset\",next_labeling) values ('%s',%i,%i,%i,%i);" tiling_name radius skip offset next_labeling) "Error adding a checkpoint:"
    )
    >>=
    \number_of_rows_inserted →
        unless (number_of_rows_inserted == 1) . error $
            printf "Wrong number of rows was inserted! (%i /= 1)" number_of_rows_inserted
-- @-node:gcross.20100728211848.1651:setCheckpoint
-- @-node:gcross.20100312175547.1817:Functions
-- @-others
-- @-node:gcross.20100312175547.1384:@thin Database.hs
-- @-leo

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
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.ConfigFile
import qualified Data.Foldable as Fold
import Data.Sequence (Seq,(|>))
import qualified Data.Sequence as Seq
import Data.UUID

import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Database.PostgreSQL.PGFunctions

import System.Exit
import System.IO.Unsafe
import System.Random

import CodeLattice.Discrete
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
-- @-node:gcross.20100312175547.1817:Functions
-- @-others
-- @-node:gcross.20100312175547.1384:@thin Database.hs
-- @-leo

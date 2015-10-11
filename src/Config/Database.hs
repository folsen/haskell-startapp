{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Config.Database where

import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import           System.Environment (lookupEnv)
import           Web.Spock.Safe

import Config.Environment

runSQL :: (HasSpock m, SpockConn m ~ DB.SqlBackend) =>
          DB.SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ DB.runSqlConn action conn
{-# INLINE runSQL #-}

parseDatabaseUrl = undefined

getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
  s <- getConnectionString e
  let n = getConnectionSize e
  case e of
    Development -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Production -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Test -> runNoLoggingT (DB.createPostgresqlPool s n)

getConnectionString :: Environment -> IO DB.ConnectionString
getConnectionString e = do
  m <- lookupEnv "DATABASE_URL"
  let s = case m of
        Nothing -> getDefaultConnectionString e
        Just u -> createConnectionString (parseDatabaseUrl u)
  return s

getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString e =
  let n = case e of
        Development -> "startapp_development"
        Production -> "startapp_production"
        Test -> "startapp_test"
  in  createConnectionString
        [ ("host", "localhost")
        , ("port", "5432")
        , ("user", "folsen")
        , ("dbname", n)
        ]

createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l =
  let f (k, v) = T.concat [k, "=", v]
  in  encodeUtf8 (T.unwords (map f l))

getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production = 8
getConnectionSize Test = 1

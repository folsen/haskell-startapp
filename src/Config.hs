{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Config (
    module Config.Database
  , Config (..)
  , getConfig
  , SessionVal
  , App
  , Action
  , IsGuest (..)
  , runApp
  , Authenticated
  , NotAuthenticated
  , ContainsGuest
  , render
  , render500
  ) where

import           Control.Monad.Logger
import qualified Database.Persist.Postgresql as DB
import           Web.Spock.Safe
import Data.HVect
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as L
import Lucid (Html (..), renderText)
import Lucid.Html5 (div_)

import Config.Database
import Config.Environment
import Models

type ContainsGuest n xs = ListContains n IsGuest xs
type NotAuthenticated xs = NotInList (UserId, User) xs ~ 'True
type Authenticated n xs = ListContains n (UserId, User) xs

data Config = Config
  { environment :: Environment
  , pool :: DB.ConnectionPool
  }

getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  p <- getPool e
  return Config
    { environment = e
    , pool = p
    }

type SessionVal = Maybe AuthSessionId
type App = SpockM DB.SqlBackend SessionVal () ()
type Action ctx a = SpockActionCtx ctx DB.SqlBackend SessionVal () a
data IsGuest = IsGuest

runApp :: App -> IO ()
runApp x = do
  pool <- runStdoutLoggingT $ DB.createPostgresqlPool (getDefaultConnectionString Development) 1
  runNoLoggingT $ DB.runSqlPool (DB.runMigration migrateAll) pool
  runSpock 8080 $ spock (spockCfg pool) x
  where
    spockCfg pool = defaultSpockCfg Nothing (PCPool pool) ()

render :: MonadIO m => Html () -> ActionCtxT ctx m a
render = html . L.toStrict . renderText

render500 :: MonadIO m => ActionCtxT ctx m a
render500 = render $ div_ "Something went wrong, please contact customer support!"

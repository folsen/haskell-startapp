{-# LANGUAGE OverloadedStrings          #-}

module Config.Environment where

import           Control.Monad.Logger
import           Data.Text.Encoding (encodeUtf8)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import           System.Environment (lookupEnv)
import           Web.Spock.Safe

import Models

getEnvironment :: IO Environment
getEnvironment = do
  m <- lookupEnv "SPOCK_ENV"
  let e = case m of
        Nothing -> Development
        Just s -> read s
  return e

data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)


getPort :: IO (Maybe Int)
getPort = do
  m <- lookupEnv "PORT"
  let p = case m of
        Nothing -> Nothing
        Just s -> Just (read s)
  return p

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import Control.Monad
import Web.Spock.Safe
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Network.HTTP.Types.Status
import Web.Spock.Digestive
import Data.HVect

import Lucid
import Lucid.Html5

import Models
import Config
import Config.Database
import qualified Controllers.Sessions as Sessions
import qualified Controllers.Users as Users

app :: IO ()
app = do
  c <- getConfig
  runApp api

api :: App
api = do
  prehook baseHook $ do
    middleware logStdoutDev

    get root $ maybeUser $ \mUser -> do
      html . L.toStrict . renderText $ do
        doctype_
        html_ $ do
          title_ "Test page"
          body_ $ do
            a_ [ href_ (renderRoute root) ] "Home"
            div_ [] $ case mUser of
                        Nothing -> do
                          a_ [href_ "/login"] "Login"
                          a_ [href_ "/register"] "Register"
                        Just user ->
                          a_ [href_ "/logout"] "Logout"

    prehook guestOnlyHook $ do
      -- getpost "/register" registerAction
      get  "/login"    Sessions.new
      post "/login"    Sessions.create

      get  "/register" Users.new
      post "/register" Users.create

    prehook authHook $ do
      get "/logout"    Sessions.destroy

baseHook :: Action () (HVect '[])
baseHook = return HNil

authHook :: Action (HVect xs) (HVect ((UserId, User) ': xs))
authHook =
    maybeUser $ \mUser ->
    do oldCtx <- getContext
       case mUser of
         Nothing ->
             noAccessPage "Unknown user. Login first!"
         Just val ->
             return (val :&: oldCtx)


guestOnlyHook :: Action (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook =
    maybeUser $ \mUser ->
    do oldCtx <- getContext
       case mUser of
         Nothing -> return (IsGuest :&: oldCtx)
         Just _  -> redirect "/"

noAccessPage :: T.Text -> Action ctx a
noAccessPage msg =
    do setStatus status403
       prefResp <- preferredFormat
       case prefResp of
         PrefJSON ->
             json (AppError msg)
         _ ->
             html msg

maybeUser :: (Maybe (UserId, User) -> Action ctx a) -> Action ctx a
maybeUser action =
    do sess <- readSession
       case sess of
         Nothing ->
             action Nothing
         Just sid ->
             do mUser <- runSQL $ loadUser sid
                action mUser

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Controllers.Sessions where

import Lucid
import Lucid.Html5
import Control.Monad
import Data.HVect
import Web.Spock.Safe

import Models
import Config
import Views.Sessions as Views

new :: (ContainsGuest n xs, NotAuthenticated xs)
    => Action (HVect xs) a
new = render $ Views.new []

create :: (ContainsGuest n xs, NotAuthenticated xs)
       => Action (HVect xs) a
create = do
  uName <- param' "email"
  uPass <- param' "password"
  loginRes <- runSQL $ loginUser uName uPass
  case loginRes of
    Just userId -> do
      sid <- runSQL $ createSession userId
      writeSession (Just sid)
      redirect "/"
    Nothing -> do
      render $ Views.new ["Wrong username or password"]

destroy :: Authenticated n xs => Action (HVect xs) a
destroy = do
  (userId, _ :: User) <- liftM findFirst getContext
  runSQL $ killSessions userId
  writeSession Nothing
  redirect "/"

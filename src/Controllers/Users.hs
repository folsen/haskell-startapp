{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Controllers.Users where

import Lucid
import Lucid.Html5
import Data.HVect
import Data.Text as T
import Web.Spock.Safe
import qualified Data.Text.Lazy as L

import Models
import Config
import Views.Users as Users

new :: (ContainsGuest n xs, NotAuthenticated xs)
    => Action (HVect xs) a
new = render $ Users.new []

create :: (ContainsGuest n xs, NotAuthenticated xs)
       => Action (HVect xs) a
create = do
  uName <- param' "email"
  uPass <- param' "password"
  registerRes <- runSQL $ registerUser uName uPass
  case registerRes of
    AppError msg -> render $ Users.new [msg]
    AppSuccess _ -> do
      loginRes <- runSQL $ loginUser uName uPass
      case loginRes of
        Just userId -> do
          sid <- runSQL $ createSession userId
          writeSession (Just sid)
          redirect "/"
        Nothing -> render500

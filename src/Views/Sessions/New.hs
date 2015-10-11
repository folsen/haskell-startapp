{-# LANGUAGE OverloadedStrings #-}
module Views.Sessions.New where

import Lucid
import Lucid.Html5
import Data.Text

new :: [Text] -> Html ()
new errors = do
  errorList errors
  form_ [method_ "POST", action_ "/login"] $ do
    input_ [type_ "text", name_ "email", placeholder_ "Email"]
    input_ [type_ "password", name_ "password", placeholder_ "Password"]
    input_ [type_ "submit", value_ "Log in"]

errorList :: [Text] -> Html ()
errorList [] = return ()
errorList errors =
  ul_ $ mapM_ (li_ . toHtml) errors


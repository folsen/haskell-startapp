{-# LANGUAGE OverloadedStrings #-}
module Models.Common where

import Data.Text
import Data.Aeson

data AppResponse
  = AppError Text
  | AppSuccess Text

instance ToJSON AppResponse where
  toJSON (AppError t)   = object [ "error" .= t ]
  toJSON (AppSuccess t) = object [ "success" .= t ]

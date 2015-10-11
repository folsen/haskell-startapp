{-# LANGUAGE OverloadedStrings #-}
module Models.Validations where

import Data.Maybe
import Text.Digestive
import qualified Lucid.Base as L
import qualified Data.Text as T

type HtmlForm m = Form T.Text m T.Text

minMaxLen :: (Int, Int) -> T.Text -> Result T.Text T.Text
minMaxLen (minLen, maxLen) t =
    if len >= minLen && len <= maxLen
    then Success stripped
    else Error $ T.pack $ "Must be longer than " ++ show minLen ++ " and shorter than " ++ show maxLen ++ " characters"
    where
      stripped = T.strip t
      len = T.length stripped

usernameFormlet :: Monad m => Maybe T.Text -> HtmlForm m
usernameFormlet mTxt =
    validate (minMaxLen (3, 12)) (text mTxt)

passwordFormlet :: Monad m => Maybe T.Text -> HtmlForm m
passwordFormlet mTxt =
    validate (minMaxLen (6, 255)) (text mTxt)

emailFormlet :: Monad m => Maybe T.Text -> HtmlForm m
emailFormlet mTxt =
    check "Not a valid email address" (isJust . T.find (== '@')) $
    validate (minMaxLen(4, 255)) (text mTxt)

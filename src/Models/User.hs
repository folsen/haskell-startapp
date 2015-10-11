{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models.User where

import Database.Persist.TH
import System.Random
import Database.Persist
import Database.Persist.Sql
import Data.Word8
import Control.Monad
import Data.Time
import Control.Monad.Trans
import GHC.Int (Int64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA512 as SHA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Models.Common

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
AuthSession
  validUntil UTCTime
  userId UserId
  deriving Show
User json
  email T.Text
  password T.Text
  salt T.Text
  deriving Show
  UniqueEmail email
|]

makeHex :: BS.ByteString -> T.Text
makeHex = T.decodeUtf8 . B16.encode
{-# INLINE makeHex #-}

decodeHex :: T.Text -> BS.ByteString
decodeHex = fst . B16.decode . T.encodeUtf8
{-# INLINE decodeHex #-}

randomBytes:: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes ct g =
    let (value, nextG) = next g
    in fromIntegral value:randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> BS.ByteString
randomBS len g =
    BS.pack $ randomBytes len g

hashPassword :: T.Text -> BS.ByteString -> BS.ByteString
hashPassword password salt =
     SHA.finalize $ SHA.updates SHA.init [salt, T.encodeUtf8 $ password]

createSession :: UserId -> SqlPersistM AuthSessionId
createSession userId =
    do now <- liftIO getCurrentTime
       insert (AuthSession (addUTCTime (5 * 3600) now) userId)

killSessions :: UserId -> SqlPersistM ()
killSessions userId =
    deleteWhere [ AuthSessionUserId ==. userId ]

loginUser :: T.Text -> T.Text -> SqlPersistM (Maybe UserId)
loginUser username password =
    do mUserE <- getBy (UniqueEmail username)
       case mUserE of
         Just userEntity ->
             let user = entityVal userEntity
             in if userPassword user == (makeHex $ hashPassword password (decodeHex $ userSalt user))
                then return $ Just (entityKey userEntity)
                else return Nothing
         Nothing ->
             return Nothing

loadUser :: AuthSessionId -> SqlPersistM (Maybe (UserId, User))
loadUser sessId =
    do mSess <- get sessId
       now <- liftIO getCurrentTime
       case mSess of
         Just sess | authSessionValidUntil sess > now -> do
           mUser <- get (authSessionUserId sess)
           return $ fmap (\user -> (authSessionUserId sess, user)) mUser
         _ ->
           return Nothing


registerUser :: T.Text -> T.Text -> SqlPersistM AppResponse
registerUser email password =
    do mUserE <- getBy (UniqueEmail email)
       case mUserE of
         Just _  ->
           return (AppError "Email already registered!")
         Nothing -> do
           g <- liftIO $ getStdGen
           let salt = randomBS 512 g
               hash = hashPassword password salt
           _ <- insert (User email (makeHex hash) (makeHex salt))
           return (AppSuccess "Signup complete. You may now login.")

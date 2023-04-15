{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Database.SQLite.Simple (Connection, execute)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as BS (unpack)
import qualified Data.ByteString.Lazy as LBS (pack)
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson hiding (Null)
import GHC.Generics

import Lib

data CreateUserDto = CreateUserDto {
  c_username :: Text,
  c_shell :: Text,
  c_home :: Text,
  c_name :: Text,
  c_phone :: Text
                                   } deriving (Generic, FromJSON)

data UpdateUserDto = UpdateUserDto {
  u_id :: Int,
  u_username :: Text,
  u_shell :: Text,
  u_home :: Text,
  u_name :: Text,
  u_phone :: Text
                                   } deriving (Generic, FromJSON)

type UpdateRow = (Text, Text, Text, Text, Text, Int)

createUser :: Connection -> CreateUserDto -> IO ()
createUser conn (CreateUserDto username_ shell_ home_ name_ phone_) =
  execute conn insertUser row
    where row :: UserRow
          row = (Null, username_, shell_, home_, name_, phone_)

updateUser :: Connection -> UpdateUserDto -> IO ()
updateUser conn (UpdateUserDto id_ username_ shell_ home_ name_ phone_) =
  execute conn "UPDATE users SET username = ?, shell = ?, homeDirectory = ?, realName = ?, phone = ? WHERE id = ?" row
    where row :: UpdateRow
          row = (username_, shell_, home_, name_, phone_, id_)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  res <- case (eitherDecode $ LBS.pack $ BS.unpack msg :: Either String UpdateUserDto) of
    Right dto -> do
      _ <- updateUser dbConn dto
      return "Updated"
    Left err ->
      case (eitherDecode $ LBS.pack $ BS.unpack msg :: Either String CreateUserDto) of
        Right dto -> do
          _ <- createUser dbConn dto
          return "Created"
        Left _ ->
          return $ "Can't deserialize request" ++ err
  sendAll soc $ encodeUtf8 $ T.pack res

handleQueries :: Connection -> Socket -> IO ()
handleQueries = handle handleQuery

main :: IO ()
main = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
      (Just (defaultHints
        {addrFlags = [AI_PASSIVE]}))
      Nothing (Just "5150")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
    Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- SQLite.open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  close sock

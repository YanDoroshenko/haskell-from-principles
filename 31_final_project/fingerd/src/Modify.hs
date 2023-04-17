{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Database.SQLite.Simple (Connection, execute)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket
import Data.Text.Encoding (encodeUtf8)
import Network.Socket.ByteString (recv, sendAll)

import Lib

main :: IO ()
main = do
  conn <- SQLite.open "finger.db"
  modifyLoop conn

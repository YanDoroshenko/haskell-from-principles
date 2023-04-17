{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQLite
import Network.Socket
import Data.Text.Encoding (decodeUtf8)
import Network.Socket.ByteString (recv)
import Control.Concurrent

import Lib

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name ->
      returnUser dbConn soc
      (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries = handle handleQuery

main :: IO ()
main = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
      (Just (defaultHints
        {addrFlags = [AI_PASSIVE]}))
      Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
    Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- SQLite.open "finger.db"
  controlThreadId <- forkIO $ modifyLoop conn
  handleQueries conn sock
  killThread controlThreadId
  SQLite.close conn
  close sock

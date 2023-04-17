{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Lib where
import Control.Exception hiding (handle)
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple hiding (close, bind)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ
import Data.Aeson hiding (Null)
import GHC.Generics
import qualified Data.ByteString as BS (unpack)
import qualified Data.ByteString.Lazy as LBS (pack)

data User = User {
    userId :: Integer
  , username :: Text
  , shell :: Text
  , homeDirectory :: Text
  , realName :: Text
  , phone :: Text
} deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
  CREATE TABLE IF NOT EXISTS users
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE,
      shell TEXT, homeDirectory TEXT,
      realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return $ Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where meRow :: UserRow
        meRow =
          (Null, "test", "/bin/zsh",
            "/home/test", "Peter Test",
            "555-123-4567")

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated =
        T.concat $
        intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  ["Login: ", e username, "\t\t\t\t",
    "Name: ", e realName, "\n",
    "Directory: ", e homeDir, "\t\t\t",
    "Shell: ", e shell, "\n"]
  where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn ("Couldn't find matching user\
                 \ for username: " ++ (show username))
      return ()
    Just user -> sendAll soc (formatUser user)

handle :: (Connection -> Socket -> IO ()) -> Connection -> Socket -> IO()
handle f dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  f dbConn soc
  close soc

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

handleModificationQuery :: Connection -> Socket -> IO ()
handleModificationQuery dbConn soc = do
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

handleModificationQueries :: Connection -> Socket -> IO ()
handleModificationQueries = handle handleModificationQuery

modifyLoop :: Connection -> IO ()
modifyLoop conn = withSocketsDo $ do
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
  handleModificationQueries conn sock
  close sock


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Aeson (Value (Null), (.=), object)
import Data.Default (def)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (Text)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Model (Application, ApplicationId, Event, EventId, migrateAll)
import Network.HTTP.Types.Status (created201, internalServerError500,
  notFound404)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings,
  setFdCacheDuration, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Environment (lookupEnv)
import Web.Heroku (parseDatabaseUrl)
import Web.Scotty.Trans (ActionT, Options, ScottyT, defaultHandler, delete,
  get, json, jsonData, middleware, notFound, param, post, put, scottyOptsT,
  settings, showError, status, verbose)

main :: IO ()
main = do
  c <- getConfig
  runApplication c

getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  p <- getPool e
  return Config
    { environment = e
    , pool = p
    }

data Config = Config
  { environment :: Environment
  , pool :: DB.ConnectionPool
  }

getEnvironment :: IO Environment
getEnvironment = do
  m <- lookupEnv "SCOTTY_ENV"
  let e = case m of
        Nothing -> Development
        Just s -> read s
  return e

data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)

getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
  s <- getConnectionString e
  let n = getConnectionSize e
  case e of
    Development -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Production -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Test -> runNoLoggingT (DB.createPostgresqlPool s n)

getConnectionString :: Environment -> IO DB.ConnectionString
getConnectionString e = do
  m <- lookupEnv "DATABASE_URL"
  let s = case m of
        Nothing -> getDefaultConnectionString e
        Just u -> createConnectionString (parseDatabaseUrl u)
  return s

getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString Development =
  "host=localhost port=5432 user=postgres dbname=event_calendar_development"
getDefaultConnectionString Production =
  "host=localhost port=5432 user=postgres dbname=event_calendar_production"
getDefaultConnectionString Test =
  "host=localhost port=5432 user=postgres dbname=event_calendar_test"

createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l =
  let f (k, v) = T.concat [k, "=", v]
  in  encodeUtf8 (T.unwords (map f l))

getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production = 8
getConnectionSize Test = 1

runApplication :: Config -> IO ()
runApplication c = do
  o <- getOptions (environment c)

  let r m = runReaderT (runConfigM m) c
  scottyOptsT o r r application

newtype ConfigM a = ConfigM
 { runConfigM :: ReaderT Config IO a
 } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

getOptions :: Environment -> IO Options
getOptions e = do
  s <- getSettings e
  return def
    { settings = s
    , verbose = case e of
      Development -> 1
      Production -> 0
      Test -> 0
    }

getSettings :: Environment -> IO Settings
getSettings e = do
  let s = defaultSettings

      s' = case e of
        Development -> setFdCacheDuration 0 s
        Production -> s
        Test -> s
  m <- getPort
  let s'' = case m of
        Nothing -> s'
        Just p -> setPort p s'
  return s''

getPort :: IO (Maybe Int)
getPort = do
  m <- lookupEnv "PORT"
  let p = case m of
        Nothing -> Nothing
        Just s -> Just (read s)
  return p

type Error = Text

application :: ScottyT Error ConfigM ()
application = do
  runDB (DB.runMigration migrateAll)
  e <- lift (asks environment)
  middleware (loggingM e)
  defaultHandler (defaultH e)

  get "/events" getEventsA
  post "/events" postEventsA
  get "/event/:id" getEventA
  put "/event/:id" putEventA
  delete "/event/:id" deleteEventA
  notFound notFoundA

runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
  DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout
loggingM Test = id

type Action = ActionT Error ConfigM ()

defaultH :: Environment -> Error -> Action
defaultH e x = do
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o

getEventsA :: Action
getEventsA = do
  ts <- runDB (DB.selectList [] [])
  json (ts :: [DB.Entity Event])

postEventsA :: Action
postEventsA = do
  t <- jsonData
  runDB (DB.insert_ t)
  status created201
  json (t :: Event)

getEventA :: Action
getEventA = do
  i <- param "id"
  m <- runDB (DB.get (toKey i))
  case m of
    Nothing -> notFoundA
    Just t -> json (t :: Event)

putEventA :: Action
putEventA = do
  i <- param "id"
  t <- jsonData
  runDB (DB.repsert (toKey i) t)
  json (t :: Event)

deleteEventA :: Action
deleteEventA = do
  i <- param "id"
  runDB (DB.delete (toKey i :: EventId))
  json Null

toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
toKey i = DB.toSqlKey (fromIntegral (i :: Integer))

notFoundA :: Action
notFoundA = do
  status notFound404
  json Null


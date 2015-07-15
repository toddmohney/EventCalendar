{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- http://taylor.fausak.me/2014/10/21/building-a-json-rest-api-in-haskell/#environment -}

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
import Network.HTTP.Types.Status (created201, internalServerError500, notFound404)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setFdCacheDuration, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Environment (lookupEnv)
import Web.Heroku (parseDatabaseUrl)
import Web.Scotty.Trans (ActionT, Options, ScottyT,
  defaultHandler, delete, get, html, json, jsonData, middleware,
  notFound, param, post, put, scottyOptsT, settings,
  showError, status, verbose)

{- import Config -}
import Model

data Config = Config
  { environment :: Environment
  , pool :: DB.ConnectionPool
  }

data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)

newtype ConfigM a 
  = ConfigM { runConfigM :: ReaderT Config IO a } 
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Error = Text
type Action = ActionT Error ConfigM ()

getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  p <- getPool e
  return Config { environment = e, pool = p }

getEnvironment :: IO Environment
getEnvironment = do
  m <- lookupEnv "SCOTTY_ENV"
  let e = case m of
        Nothing -> Development
        Just s -> read s
  return e

getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
  s <- getConnectionString e
  let n = getConnectionSize e
  case e of
    Development -> runStdoutLoggingT
      (DB.createPostgresqlPool s n)
    Production -> runStdoutLoggingT
      (DB.createPostgresqlPool s n)
    Test -> runNoLoggingT
      (DB.createPostgresqlPool s n)

getConnectionString :: Environment -> IO DB.ConnectionString
getConnectionString e = do
  m <- lookupEnv "DATABASE_URL"
  let s = case m of
        Nothing -> getDefaultConnectionString e
        Just u -> createConnectionString (parseDatabaseUrl u)
  return s

getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString e =
  let n = case e of
        Development -> "event_calendar_development"
        Production  -> "event_calendar_production"
        Test        -> "event_calendar_test"
  in  createConnectionString
        [ ("host", "localhost")
        , ("port", "5432")
        , ("user", "postgres")
        , ("dbname", n)
        ]

createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l =
  let f (k, v) = T.concat [k, "=", v]
  in  encodeUtf8 (T.unwords (map f l))

getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production = 8
getConnectionSize Test = 1

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

runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
  DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout
loggingM Test = id

defaultH :: Environment -> Error -> Action
defaultH e x = do
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o

runApplication :: Config -> IO ()
runApplication c = do
  o <- getOptions (environment c)
  let r m = runReaderT (runConfigM m) c
  scottyOptsT o r r application

application :: ScottyT Error ConfigM ()
application = do
  runDB (DB.runMigration migrateAll)
  e <- lift (asks environment)
  middleware (loggingM e)
  defaultHandler (defaultH e)

  get "/" $ html "Hello World!"
  get "/events" $ html "Here are all of your application's events"
  get "/event/:eventId" $ html "Here's that application event you asked for"
  get "/organizers/:organizerId/events" $ html "Here are all of the events created by this organizer"
  notFound notFoundA

{- getEventsA :: Action -}
{- getEventsA = do -}
  {- ts <- runDB (DB.selectList [] []) -}
  {- json (ts :: [DB.Entity Event]) -}

notFoundA :: Action
notFoundA = do
  status notFound404
  json Null

main :: IO ()
main = do
  c <- getConfig
  runApplication c


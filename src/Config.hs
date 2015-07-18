{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Default (def)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Network.Wai.Handler.Warp (Settings, defaultSettings, setFdCacheDuration, setPort)
import System.Environment (lookupEnv)
import Web.Heroku (parseDatabaseUrl)
import Web.Scotty.Trans (Options, scottyOptsT, settings, verbose)

data Config = Config
  { environment :: Environment
  , pool :: DB.ConnectionPool
  }

data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)

newtype ConfigM a = ConfigM
 { runConfigM :: ReaderT Config IO a
 } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  p <- getPool e
  return Config
    { environment = e
    , pool = p
    }

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


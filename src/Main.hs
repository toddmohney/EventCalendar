{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Aeson (Value (Null), (.=), object)
import Data.Text.Lazy (Text)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Network.HTTP.Types.Status (created201, internalServerError500,
  notFound404)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Web.Scotty.Trans (ActionT, Options, ScottyT, defaultHandler, delete,
  get, json, jsonData, middleware, notFound, param, post, put, scottyOptsT,
  settings, showError, status)

import Config
import Model (Application, ApplicationId, Event, EventId, migrateAll)

type Error = Text

main :: IO ()
main = getConfig >>= runApplication

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


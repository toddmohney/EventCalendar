{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where
  import Data.Aeson (Value (Null))

  import Control.Monad.IO.Class  (liftIO)
  import Control.Monad.Logger    (runStderrLoggingT)

  import Data.Maybe (fromMaybe)
  import Data.Monoid ((<>))
  import qualified Data.Text.Lazy as T
  import Data.ByteString.Char8 (pack)

  import Database.Persist as DB
  import Database.Persist.Postgresql as DB
  import Database.Persist.TH

  import Network.HTTP.Types.Status (notFound404)
  import Network.Wai.Middleware.RequestLogger(logStdoutDev)

  import qualified Web.Scotty as S
  import Web.Scotty.Internal.Types (ActionT)

  import Config
  import Model
  import ModelSerializer

  connStr :: ConnectionString
  connStr = "host=localhost dbname=event_calendar user=toddmohney port=5432"

  inHandlerDb = liftIO . dbFunction
  inAppDb     = liftIO . dbFunction

  dbFunction query = runStderrLoggingT $
          withPostgresqlPool connStr 10 $
          \pool -> liftIO $ runSqlPersistMPool query pool

  doMigrations = runMigration migrateAll

  notFound = do
    S.status notFound404
    S.json Null

  toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
  toKey i = DB.toSqlKey (fromIntegral (i :: Integer))

  getEvents :: ActionT T.Text IO ()
  getEvents = do
    events <- inHandlerDb $ DB.selectList [] []
    S.json (events :: [Entity Event])

  getEvent :: ActionT T.Text IO ()
  getEvent = undefined
  {- getEvent = do -}
    {- id    <- S.param "id" -}
    {- event <- inHandlerDb $ DB.get (toKey (read id) :: EventId) -}
    {- case event of -}
      {- Just e  -> S.json e -}
      {- Nothing -> notFound -}

  main :: IO ()
  main = S.scotty 3000 $ do
          S.middleware logStdoutDev
          inAppDb doMigrations
          S.get "/" $ S.html "Hello World"
          S.get "/events" getEvents
          S.get "/event/:id" getEvent


{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where
  import Control.Monad.IO.Class  (liftIO)
  import Control.Monad.Logger    (runStderrLoggingT)
  import Data.Monoid ((<>))
  import qualified Data.Text.Lazy as T

  import Database.Persist
  import Database.Persist.Postgresql
  import Database.Persist.TH

  import qualified Web.Scotty as S
  import Network.Wai.Middleware.RequestLogger(logStdoutDev)

  import Model
  import ModelSerializer

  connStr :: ConnectionString
  connStr = "host=localhost dbname=event_calendar user=toddmohney port=5432"

  main :: IO ()
  main = S.scotty 3000 $ do
          S.middleware logStdoutDev
          inAppDb doMigrations
          S.get "/" $ S.html "Hello World"
          S.get "/posts" $ do
              events :: [Entity Event] <- inHandlerDb $ selectList [] []
              S.json events

  inHandlerDb = liftIO . dbFunction
  inAppDb     = liftIO . dbFunction

  dbFunction query = runStderrLoggingT $
          withPostgresqlPool connStr 10 $
          \pool -> liftIO $ runSqlPersistMPool query pool

  doMigrations = runMigration migrateAll


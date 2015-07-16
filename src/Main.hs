{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import qualified Database.Persist.Postgresql as DB
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Data.ByteString.Char8 (pack)
import qualified Web.Scotty as S

import Config
import Model
import ModelSerializer

connPoolSize :: Int
connPoolSize = 4

runDb = liftIO . dbFunction

dbFunction :: DB.SqlPersistM () -> IO ()
dbFunction query = do
  connStr <- postgresConnectionString
  runStderrLoggingT $
    DB.withPostgresqlPool (pack connStr) connPoolSize $
      \pool -> liftIO $ DB.runSqlPersistMPool query pool

{- getEventsA = do -}
  {- events <- runDb (DB.selectList [] []) -}
  {- S.json (events :: [DB.Entity Event]) -}

main :: IO ()
main = S.scotty 3000 $ do
  S.middleware logStdoutDev
  runDb $ DB.runMigration migrateAll
  S.get "/" $ S.html "Hello World!"
  S.get "/events" $ S.html "Hello World!"
  S.get "/event/:eventId" $ S.html "Here's that application event you asked for"
  S.get "/organizers/:organizerId/events" $ S.html "Here are all of the events created by this organizer"


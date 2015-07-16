{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Data.ByteString.Char8 (pack)
import qualified Web.Scotty as S

import Config
import Model

connPoolSize = 4

runDb = liftIO . dbFunction

dbFunction query = do
  connStr <- postgresConnectionString
  runStderrLoggingT $
    withPostgresqlPool (pack connStr) connPoolSize $
      \pool -> liftIO $ runSqlPersistMPool query pool

main :: IO ()
main = S.scotty 3000 $ do
  S.middleware logStdoutDev
  runDb $ runMigration migrateAll
  S.get "/" $ S.html "Hello World!"
  S.get "/events" $ S.html "Here are all of your application's events"
  S.get "/event/:eventId" $ S.html "Here's that application event you asked for"
  S.get "/organizers/:organizerId/events" $ S.html "Here are all of the events created by this organizer"


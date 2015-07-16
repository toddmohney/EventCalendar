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

import Data.ByteString.Char8 (pack)
import qualified Web.Scotty as S

import Config
import Model

connPoolSize = 10

main = do
  connStr <- postgresConnectionString
  runStderrLoggingT $ withPostgresqlPool (pack connStr) connPoolSize $ \pool -> liftIO $
    flip runSqlPersistMPool pool $ runMigration migrateAll

  S.scotty 3000 $ do
    S.get "/" $ S.html "Hello World!"
    S.get "/events" $ S.html "Here are all of your application's events"
    S.get "/event/:eventId" $ S.html "Here's that application event you asked for"
    S.get "/organizers/:organizerId/events" $ S.html "Here are all of the events created by this organizer"




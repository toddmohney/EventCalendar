{-# LANGUAGE OverloadedStrings          #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import Data.ByteString.Char8 (pack)
import Web.Scotty as S

import Config

connPoolSize = 10

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]


main = do
  connStr <- postgresConnectionString
  runStderrLoggingT $ withPostgresqlPool (pack connStr) connPoolSize $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ runMigration migrateAll

  S.scotty 3000 $ do
    S.get "/" $ html "Hello World!"




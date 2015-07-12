{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where
  import Database.Persist()
  import Database.Persist.TH
  import Data.Time

  share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Application
    name String
    accessKey String
    secretAccessKey String
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show
  |]

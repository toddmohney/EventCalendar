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
    Event
      ownerId Int
      name String
      description String Maybe default=''
      location String default=''
      startDate UTCTime
      endDate UTCTime
      timeZone String
      private Bool
      cancelled Bool default=false
      updateCount Int default=0
      generatesNotifications Bool default=true
      createdAt UTCTime default=now()
      updatedAt UTCTime default=now()
  |]

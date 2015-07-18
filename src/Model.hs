{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}

module Model where
  import Database.Persist()
  import Database.Persist.TH
  import Data.Time

  share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Application json sql=applications
      name String
      createdAt UTCTime default=now()
      updatedAt UTCTime default=now()
      deriving Show
    Event json sql=events
      applicationId ApplicationId
      organizerId Int
      name String
      description String Maybe default=''
      location String default=''
      startDate UTCTime
      endDate UTCTime
      timeZone String
      cancelled Bool default=false
      createdAt UTCTime default=now()
      updatedAt UTCTime default=now()
      deriving Show
  |]

{- Constraints needed: -}
{- Event -}
  {- applicationId -}

{- Indexes needed: -}
{- Event -}
  {- applicationId -}
  {- (applicationId, organizerId) -}
  {- startDate -}
  {- endDate -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
 
{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}
 
module ModelSerializer where
  import           Control.Applicative
  import           Control.Monad
  import           Data.Aeson
  import           Database.Persist
  import           Model

  instance ToJSON (Entity Event) where
    toJSON (Entity id (Event{..})) =
      object
      [ "id"                     .= id
      , "applicationId"          .= eventApplicationId
      , "organizerId"            .= eventOrganizerId
      , "name"                   .= eventName
      , "description"            .= eventDescription
      , "location"               .= eventLocation
      , "startDate"              .= eventStartDate
      , "endDate"                .= eventEndDate
      , "timeZone"               .= eventTimeZone
      , "private"                .= eventPrivate
      , "cancelled"              .= eventCancelled
      , "updateCount"            .= eventUpdateCount
      , "generatesNotifications" .= eventGeneratesNotifications
      , "createdAt"              .= eventCreatedAt
      , "updatedAt"              .= eventUpdatedAt
      ]
   
  instance FromJSON Event where
      parseJSON (Object v) =
        Event <$> v .: "applicationId"
          <*> v .: "organizerId"
          <*> v .: "name"
          <*> v .: "description"
          <*> v .: "location"
          <*> v .: "startDate"
          <*> v .: "endDate"
          <*> v .: "timeZone"
          <*> v .: "private"
          <*> v .: "cancelled"
          <*> v .: "updateCount"
          <*> v .: "generatesNotifications"
          <*> v .: "createdAt"
          <*> v .: "updatedAt"
      parseJSON _ = mzero

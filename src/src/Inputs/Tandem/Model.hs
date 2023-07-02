{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Inputs.Tandem.Model where

import GHC.Generics
import Data.Aeson
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T

data DeliveryStatus
  = Open                                  -- "open"
  | Other                                 -- Everything else
  deriving (Generic, Show)

instance FromJSON DeliveryStatus where
  parseJSON = withText "DeliveryStatus" $ \v ->
    case v of
      "open" -> return Open
      _ -> return Other

data MessageAttachment =
  MessageAttachment
    { original :: LogEntryMessage
    , comment :: Text
    , attachmentType :: Text -- Label "type", "comment"
    }
    deriving (Generic, Show)

instance FromJSON MessageAttachment where
  parseJSON = withObject "MessageAttachment" $ \v ->
    MessageAttachment
      <$> v .: "original"
      <*> v .: "comment"
      <*> v .: "type"

data LogEntryMessage =
  LogEntryMessage
    { content :: Maybe Text
    , attachment :: Maybe MessageAttachment
    , self :: Text                        -- "inc_messaging_usermsg" / "inc_messaging_usermsgwdata"
    }
    deriving (Generic, Show)

instance FromJSON LogEntryMessage where
  parseJSON = withObject "LogEntryMessage" $ \v ->
    LogEntryMessage
      <$> v .:? "content"
      <*> v .:? "attachment"
      <*> v .: "self"

data LogEntry =
  LogEntry
    { deliveryId :: Text
    , deliveryStatus :: DeliveryStatus
    , deliveryStatusDt :: Text            -- "2023-06-03T14:14:30+00:00"
    , toEntityId :: Int                   -- 11111111
    , flow :: Text                        -- "in"
    , timestamp :: Text                   -- "2023-06-03T14:05:41+00:00"
    , logEntryMessage :: LogEntryMessage  -- Labeled "_"
    }
    deriving (Generic, Show)

instance FromJSON LogEntry where
  parseJSON = withObject "LogEntry" $ \v ->
    LogEntry
      <$> v .: "deliveryId"
      <*> v .: "deliveryStatus"
      <*> v .: "deliveryStatusDt"
      <*> v .: "toEntityId"
      <*> v .: "flow"
      <*> v .: "timestamp"
      <*> v .: "_"

data UserDetails =
  UserDetails
    { first_name :: Text                 -- Unicode Support Req
    , great_talks :: Int
    , img :: Text                        -- URL
    , onlineStatus :: Text               -- "online_video"
    , onlineDate :: Text                 -- "2023-06-04T02:05:14+00:00"
    , directCallTopicId :: Maybe Int     -- null
    , callbackTopicId :: Maybe Int       -- null
    , age :: Int
    , isFollowed :: Bool
    , isNearMe :: Bool
    , isFollowingMe :: Bool
    , gender :: Text                   -- "M"|"F"
    -- , tutorRequestStatus :: Maybe null
    -- , tutorType ::null
    -- , allowGift ::true
    -- , canLeaveReference ::false
    -- , canEditReference ::false
    -- , isCallAllowed ::true
    }
    deriving (Generic, Show)

instance FromJSON UserDetails where
  parseJSON = withObject "UserDetails" $ \o ->
    UserDetails
      <$> o .: "first_name"
      <*> o .: "great_talks"
      <*> o .: "img"
      <*> o .: "onlineStatus"
      <*> o .: "onlineDate"
      <*> o .:? "directCallTopicId"
      <*> o .:? "callbackTopicId"
      <*> o .: "age"
      <*> o .: "isFollowed"
      <*> o .: "isNearMe"
      <*> o .: "isFollowingMe"
      <*> o .: "gender"
      -- <*> o .:? "tutorRequestStatus"
      -- <*> o .:? "tutorType"
      -- <*> o .: "allowGift"
      -- <*> o .: "canLeaveReference"
      -- <*> o .: "canEditReference"
      -- <*> o .: "isCallAllowed"

data EntityMetadata =
  EntityMetadata
      { entityId :: Int
      , entityType :: Text            -- "user"
      }
    deriving (Generic, Show)

instance FromJSON EntityMetadata where
  parseJSON (Object o) =
    EntityMetadata
    <$> o .: "entityId"
    <*> o .: "entityType"
  parseJSON _ = fail "Invalid EntityMetadata JSON"

data ConversationEntity =
  ConversationEntity
      { entity :: EntityMetadata
      , details :: UserDetails
      }
    deriving (Generic, Show)

instance FromJSON ConversationEntity where
  parseJSON (Object o) =
    ConversationEntity
    <$> o .: "entity"
    <*> o .: "details"
  parseJSON _ = fail "Invalid ConversationEntity JSON"

data ResponseConversation =
  ResponseConversation
    { opponent :: ConversationEntity
    , log :: [LogEntry]
    }
    deriving (Generic, Show)

instance FromJSON ResponseConversation where
  parseJSON (Object o) =
    ResponseConversation
    <$> o .: "opponent"
    <*> o .: "log"
  parseJSON _ = fail "Invalid ResponseConversation JSON"

data ResponseEnvelope =
  ResponseEnvelope
    { envelopeResponse :: ResponseConversation  -- Labeled "response"
    -- , errorCode :: null
    , envelopeType :: Text                    -- Labeled "type", val "success"
    }
    deriving (Generic, Show)

instance FromJSON ResponseEnvelope where
  parseJSON (Object o) =
    ResponseEnvelope
    <$> o .: "response"
    <*> o .: "type"
  parseJSON _ = fail "Invalid ResponseEnvelope JSON"


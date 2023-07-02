{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Translator.OpenAPI.GPT
  (postToApi)
  where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Maybe as Maybe
import           GHC.Generics
import           Network.HTTP.Req
import           Network.HTTP.Client hiding (responseBody)
import           Network.HTTP.Client.TLS
import           System.Environment

myManager :: IO Manager
myManager = do
  newManager $
    tlsManagerSettings { managerWrapException = wrapException }

wrapException :: Request -> IO a -> IO a
wrapException _req ioOp = do
  --- Nothing for now.
  ioOp

apiKey :: IO Text
apiKey = do
  getEnv "GPT_KEY" >>= return . T.pack

gptModel :: Text
gptModel = "text-davinci-003"

data CompletionRequest =
  CompletionRequest
    { model :: Text
    , prompt :: Text
    , max_tokens :: Int
    , temperature :: Int -- Really a double, but we're only ever passing in 0.
    }
    deriving (Generic, Show, ToJSON)

mintReq :: Text -> CompletionRequest
mintReq korean =
  CompletionRequest
    { model = gptModel
    , prompt = "What does the following korean mean in english? If you can translate it, respond with \"The text means: \" and then the translation. If you cannot translate it, only respond with \"Translation failed\" and nothing else. \n\n" <> korean
    , max_tokens = 200
    , temperature = 0
    }

successDelimiter :: Text
successDelimiter = "The text means: "

data CompletionResponse =
  CompletionResponse
    { completion_id :: Text
    , completion_object :: Text
    , completion_created :: Int
    , completion_model :: Text
    , completion_choices :: [CompletionResponseChoice]
    , completion_usage :: CompletionResponseUsage
    }
    deriving (Generic,Show)
instance FromJSON CompletionResponse where
  parseJSON = withObject "CompletionResponse" $ \v ->
    CompletionResponse
      <$> v .: "id"
      <*> v .: "object"
      <*> v .: "created"
      <*> v .: "model"
      <*> v .: "choices"
      <*> v .: "usage"

data CompletionResponseUsage =
  CompletionResponseUsage
    { usage_promptTokens :: Int
    , usage_completionTokens :: Int
    , usage_totalTokens :: Int
    }
    deriving (Generic,Show)
instance FromJSON CompletionResponseUsage where
  parseJSON = withObject "CompletionResponseUsage" $ \v ->
    CompletionResponseUsage
      <$> v .: "prompt_tokens"
      <*> v .: "completion_tokens"
      <*> v .: "total_tokens"

data CompletionResponseChoice =
  CompletionResponseChoice
    { choice_text :: Text
    , choice_index :: Int
    , choice_logprobs :: Maybe Int
    , choice_finish_reason :: Text
    }
    deriving (Generic,Show)
instance FromJSON CompletionResponseChoice where
  parseJSON = withObject "CompletionResponseChoice" $ \v ->
    CompletionResponseChoice
      <$> v .: "text"
      <*> v .: "index"
      <*> v .: "logprobs"
      <*> v .: "finish_reason"

postToApi :: IO (Either String Text)
postToApi = do
  manager <- myManager
  runReq defaultHttpConfig { httpConfigAltManager = Just manager } $ do
    key <- liftIO apiKey

    r <-
      req
        POST
        (https "api.openai.com" /: "v1" /: "completions")
        (ReqBodyJson $ mintReq "다른 언어로 대화하는게")
        jsonResponse
        (mempty
        <> header "Authorization" (T.encodeUtf8 $ "Bearer " <> key)
        )

    let
      ei_responseEnv =
        (parseEither parseJSON $ responseBody r :: Either String CompletionResponse)
        >>= parseOutTranslation
    case ei_responseEnv of
      Left e -> liftIO $ do
        putStrLn "Failed to parse!"
        putStrLn e
      Right v -> liftIO $ do
        putStrLn "Parse success!"
        putStrLn $ show v

    return ei_responseEnv

parseOutTranslation :: CompletionResponse -> Either String Text
parseOutTranslation completionResponse =
  case completion_choices completionResponse of
    [] ->
      Left "No choices returned"
    (c:_) ->
      case T.breakOn successDelimiter $ choice_text c of
        (_, "") ->
          failedTranslation
        (_, translation) ->
          Maybe.maybe failedTranslation Right $ T.stripPrefix successDelimiter translation
    where
      failedTranslation = Left "Translation failed"


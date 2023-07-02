{-# LANGUAGE OverloadedStrings #-}
module Inputs.Tandem.Api where

import           Data.Aeson
import           Data.Aeson (Value)
import           Data.Aeson.Types
import           Control.Exception (catch, throwIO, SomeException)
import           Control.Monad.IO.Class
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString as BS
import           Data.ByteString.UTF8 as BSU
import           Network.HTTP.Req
import           Network.HTTP.Client hiding (responseBody)
import           Network.HTTP.Client.TLS
import           Network.TLS

import           Inputs.Tandem.Session (Session)
import qualified Inputs.Tandem.Session as Session
import           Inputs.Tandem.Model as Model

myManager :: IO Manager
myManager = do
  newManager $
    tlsManagerSettings { managerWrapException = wrapException }

wrapException :: Request -> IO a -> IO a
wrapException req ioOp = do
  --- Nothing for now.
  ioOp

endpointListChats :: Text
endpointListChats = "v1/messages#list"

endpointGetChatMessages :: Text
endpointGetChatMessages = "v1/messages#get"

postToApi :: Session -> IO (Either String ResponseEnvelope)
postToApi session = do
  manager <- myManager
  runReq defaultHttpConfig { httpConfigAltManager = Just manager } $ do
    session <- liftIO Session.getSession

    r <-
      req
        POST
        (https "web-apis.tandem.net" /: "api" /: "app")
        (ReqBodyBs $ Session.encodeToBs session)
        jsonResponse
        mempty

    let
      ei_responseEnv = parseEither parseJSON $ responseBody r :: Either String ResponseEnvelope
    case ei_responseEnv of
      Left e -> liftIO $ do
        putStrLn "Failed to parse!"
        putStrLn e
      Right v -> liftIO $
        putStrLn "Parse success!"

    return ei_responseEnv

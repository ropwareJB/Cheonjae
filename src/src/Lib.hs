{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    ) where

import Control.Exception (catch, throwIO, SomeException)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson (Value)
import Data.Aeson.Types
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU
import Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Hex as T
import Network.HTTP.Req
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS
import Network.TLS
import System.Environment

import Tandem.Model

sessionToken :: IO Text
sessionToken = do
  getEnv "TANDEM_SESSION" >>= return . T.pack

decodeToken :: Text -> Maybe ByteString
decodeToken = T.decodeHex

myManager :: IO Manager
myManager = do
  newManager $
    tlsManagerSettings { managerWrapException = wrapException }

wrapException :: Request -> IO a -> IO a
wrapException req ioOp = do
  --- Nothing for now.
  ioOp

main :: IO ()
main = do
  manager <- myManager
  runReq defaultHttpConfig { httpConfigAltManager = Just manager } $ do
    session <- liftIO sessionToken
    r <-
      req
        POST
        (https "web-apis.tandem.net" /: "api" /: "app")
        (ReqBodyBs $ T.encodeUtf8 session)
        jsonResponse
        mempty

    case (parseEither parseJSON $ responseBody r :: Either String ResponseEnvelope) of
      Left e -> liftIO $ do
        putStrLn "Failed to parse!"
        putStrLn e
      Right v -> liftIO $
        putStrLn "Parse success!"

    liftIO $ print (responseBody r)
  -- return ()

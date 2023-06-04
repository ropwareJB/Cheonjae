{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU
import Network.HTTP.Req
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS
import Control.Exception (catch, throwIO, SomeException)
import Network.TLS
import System.Environment

import Data.Aeson (Value)
import Data.Map as Map

sessionToken :: IO String
sessionToken = do
  getEnv "TANDEM_SESSION"

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
        (ReqBodyBs $ BSU.fromString session)
        lbsResponse
        mempty

    liftIO $ print (responseBody r)
  -- return ()

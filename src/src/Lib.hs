{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    ) where

import Data.Map as Map

import qualified Tandem.Model as TModel
import qualified Tandem.Session as TSession
import qualified Tandem.Api as TApi

main :: IO ()
main = do
  session <- TSession.getSession
  ei_responseEnvelope <- TApi.postToApi session
  return ()

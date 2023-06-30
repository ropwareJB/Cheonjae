{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    ) where

import Data.Map as Map

import qualified Tandem.Model as TModel
import qualified Tandem.Session as TSession
import qualified Tandem.Api as TApi

import Text.Printf
import Data.Text as T
import Data.Text.IO as T

import qualified OpenAPI.GPT as ChatGPT

import Args

main :: Args -> IO ()
main args@ArgsDigest{} = do
  -- session <- TSession.getSession
  -- ei_responseEnvelope <- TApi.postToApi session

  -- TODO: use sqlite-simple package to inspect anki DBs
  words <- readData $ Args.input args
  mapM_ (printf "%s\n") words

  ChatGPT.postToApi
  return ()

readData :: FilePath -> IO [Text]
readData fPath = do
  f <- T.readFile fPath
  return $ T.words f


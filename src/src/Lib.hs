{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    ) where

import Text.Printf

import Args
import qualified Inputs.FlatFile as InputFlatFile
import qualified Translator.OpenAPI.GPT as ChatGPT
import qualified Stores.Anki.Anki as Anki

main :: Args -> IO ()
main args@ArgsDigest{} = do
  -- TODO: use sqlite-simple package to inspect anki DBs
  words <- InputFlatFile.readCards $ Args.input args

  mapM_ (printf "%s\n") words

  ChatGPT.postToApi
  return ()


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

  cards <- InputFlatFile.readCards $ Args.input args

  anki <- Anki.open "/home/jb/vmshare/test/collection.anki21"
  partioned_ei <- Anki.partition anki cards
  case partioned_ei of
    Left e ->
      putStrLn e
    Right (notesAlreadyPresent, notesToBeTranslated) -> do
      printf "Notes already present: %d\n" $ length notesAlreadyPresent
      printf "Translating %d cards...\n" $ length notesToBeTranslated
      mapM_ (printf "%s\n") notesToBeTranslated

  -- translations <- mapM ChatGPT.postToApi cards
  -- TODO: Store successful translations
  -- TODO: Store unsuccessful translatinos somewhere + log
  return ()


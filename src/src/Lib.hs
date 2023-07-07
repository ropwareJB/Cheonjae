{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    ) where

import Text.Printf
import qualified Data.Either as Either

import Args
import qualified Model
import qualified Inputs.FlatFile as InputFlatFile
import qualified Translator.OpenAPI.GPT as ChatGPT
import qualified Stores.Anki.Anki as Anki

main :: Args -> IO ()
main args@ArgsDigest{} = do

  cards <- InputFlatFile.readCards $ Args.input args

  anki <- Anki.open $ Args.ankiStore args
  partioned_ei <- Anki.partition anki cards
  case partioned_ei of
    Left e ->
      putStrLn e
    Right (notesAlreadyPresent, notesToBeTranslated) -> do
      printf "Notes already present: %d\n" $ length notesAlreadyPresent
      printf "Translating %d cards...\n" $ length notesToBeTranslated
      mapM_ (printf "%s\n") notesToBeTranslated

      newCards_eis <- mapM
        (\n -> do
          back_ei <- ChatGPT.postToApi n
          return $
            Either.either
              (\err -> Left $ (n, err))
              (\back -> Right $ Model.MCard n back)
              back_ei
        )
        notesToBeTranslated

      let
        translateSuccesses = Either.rights newCards_eis
        translateFails = Either.lefts newCards_eis

      -- Store successful translations
      putStrLn $ show newCards_eis
      success <- mapM (Anki.storeNewNote anki) translateSuccesses

      -- TODO: Store unsuccessful translatinos somewhere + log
      return ()

  _ <- Anki.close anki
  return ()


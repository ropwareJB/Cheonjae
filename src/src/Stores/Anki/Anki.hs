{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Stores.Anki.Anki
  ( Stores.Anki.Anki.open
  , Stores.Anki.Anki.close
  , Stores.Anki.Anki.partition
  , readNotes
  , storeNewNote
  ) where

import           GHC.Generics
import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List
import           Data.Maybe as Maybe
import           Database.SQLite.Simple as SQL
import           Database.SQLite.Simple.FromRow

import qualified Model

data AnkiStore
  = AnkiStoreClosed
      {}
  | AnkiStoreOpen
      { conn :: Connection }

data ModelNote =
  ModelNote
    { noteId :: Int
    , noteGuid :: Text
    , noteMid :: Int
    , noteMod :: Int
    , noteUsn :: Int
    , noteTags :: Text
    , noteFlds :: Text
    , noteSfld :: Text
    , noteCsum :: Int
    , noteFlags :: Int
    , noteData :: Text
    }
  deriving (Show, Generic, FromRow, ToRow)

-- | Given a FilePath, load an Anki21 data store
open :: FilePath -> IO AnkiStore
open fp = do
  conn <- SQL.open fp
  return $ AnkiStoreOpen conn

-- | Close a given AnkiStore, if opened.
close :: AnkiStore -> IO AnkiStore
close AnkiStoreOpen{ conn } = do
  SQL.close conn
  return AnkiStoreClosed
close s@AnkiStoreClosed =
  return s

-- | Parition the set of cards into a pair:
-- - Already translated and stored (left)
-- - To be translated (right)
partition :: AnkiStore -> [Text] -> IO (Either String ([Text],[Text]))
partition s@AnkiStoreClosed{} _ =
  return $ Left "Anki store not open."
partition s@AnkiStoreOpen{} rawNotes = do
  modelNotes <- readNotes s
  return . Right $
    Data.List.partition
      (\rawNote ->
        Maybe.isJust $ Data.List.find (\modelNote -> (noteSfld modelNote) == rawNote ) modelNotes
      )
      rawNotes

-- | Read all the Note models out from the database.
readNotes :: AnkiStore -> IO [ModelNote]
readNotes s@AnkiStoreClosed{} =
  -- TODO: Throw / log error
  return []
readNotes s@AnkiStoreOpen{} = do
  r <- query_ (conn s) "SELECT * FROM notes" :: IO [ModelNote]
  mapM_ (print . show) r
  return r

storeNewNote :: AnkiStore -> Model.MCard -> IO()
storeNewNote s@AnkiStoreClosed{} _ =
  -- TODO: Throw / log error
  return ()
storeNewNote s@AnkiStoreOpen{} newCard = do
  -- TODO: store
  return ()

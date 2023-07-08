{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Stores.Anki.Anki
  ( Stores.Anki.Anki.open
  , Stores.Anki.Anki.close
  , Stores.Anki.Anki.partition
  , readNotes
  , readLastNote
  , storeNewNote
  ) where

import           GHC.Generics
import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.List
import           Data.Maybe as Maybe
import           Database.SQLite.Simple as SQL
import           Database.SQLite.Simple.FromRow
import           Text.Printf

import qualified Model

data AnkiStore
  = AnkiStoreClosed
      {}
  | AnkiStoreOpen
      { conn :: Connection }

data ModelCard =
  ModelCard
    { cardId :: Int
    , cardNid :: Int
    , cardDid :: Int
    , cardOrd :: Int
    , cardMod :: Int
    , cardUsn :: Int
    , cardType :: Int
    , cardQueue :: Int
    , cardDue :: Int
    , cardIvl :: Int
    , cardFactor :: Int
    , cardReps :: Int
    , cardLapses :: Int
    , cardLeft :: Int
    , cardOdue :: Int
    , cardFlags :: Int
    , cardData :: Text
    }

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

readLastNote :: AnkiStore -> IO (Maybe ModelNote)
readLastNote s@AnkiStoreClosed{} =
  -- TODO: Throw / log error
  return Nothing
readLastNote s@AnkiStoreOpen{} = do
  rs <- query_ (conn s) "SELECT * FROM notes ORDER BY id DESC LIMIT 1" :: IO [ModelNote]
  case rs of
    []    -> return Nothing
    (n:_) -> return $ Just n

storeNewNote :: AnkiStore -> Model.MCard -> IO()
storeNewNote s@AnkiStoreClosed{} _ = do
  -- TODO: Throw / log error
  putStrLn "[!] Anki store is closed!"
  return ()
storeNewNote s@AnkiStoreOpen{} newCard = do
  -- Retrieve an existing card, to copy attributes from.
  printf "[+] Storing card! %s" (Model.front newCard)

  lastNote_myb <- readLastNote s
  mapM
    (\lastNote -> do
      -- Store a standalone Note
      executeNamed
        (conn s)
        "INSERT INTO notes \
          \        (guid, mid, mod, usn, tags, flds, sfld, csum, flags, data) \
          \ VALUES (:guid, :mid, :mod, :usn, :tags, :flds, :sfld, :csum, :flags, :data)"
          ([ ":guid" := ("" :: T.Text) -- TODO: Generate new GUID
           , ":mid" := (123 :: Int) -- TODO: Copy from other
           , ":mod" := (123 :: Int) -- TODO: What is this?
           , ":usn" := (6 :: Int)
           , ":tags" := ("" :: T.Text)
           , ":flds" := (T.pack $ printf "%s\US%s" (Model.front newCard) (Model.back newCard) :: T.Text)
           , ":sfld" := (T.pack $ printf "%s" (Model.front newCard) :: T.Text)
           , ":csum" := (123 :: Int)
           , ":flags" := (0 :: Int)
           , ":data" := ("" :: T.Text)
           ])
      insertedNoteId <- lastInsertRowId (conn s)

      -- TODO: Get the last Card in the DB to template off.

      -- Store a Card reference in a Deck that links to our new Note
      executeNamed
        (conn s)
        "INSERT INTO cards \
          \        (nid, did, ord, mod, usn, type, queue, due, ivl, factor, reps, lapses, left, odue, odid, flags, data) \
          \ VALUES (:nid, :did, :ord, :mod, :usn, :type, :queue, :due, :ivl, :factor, :reps, :lapses, :left, :odue, :odid, :flags, :data) "
          ([ ":nid" := insertedNoteId
           , ":did" := (123 :: Int) -- TODO: Copy from other
           , ":ord" := (0 :: Int)
           , ":mod" := (123 :: Int) -- TODO: What is this?
           , ":usn" := (6 :: Int) -- TODO: What is this?
           , ":type" := (0 :: Int)
           , ":queue" := (0 :: Int)
           , ":due" := (123 :: Int) -- TODO Incrememnt from previous card
           , ":ivl" := (0 :: Int)
           , ":factor" := (0 :: Int)
           , ":reps" := (0 :: Int)
           , ":lapses" := (0 :: Int)
           , ":left" := (0 :: Int)
           , ":odue" := (0 :: Int)
           , ":flags" := (0 :: Int)
           , ":data" := ("{}" :: T.Text)
           ])
    )
    lastNote_myb

  return ()

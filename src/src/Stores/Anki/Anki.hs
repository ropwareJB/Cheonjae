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
import qualified Codec.Binary.Base91 as Base91
import           Control.Applicative
import           Control.Monad.Extra
import qualified Data.ByteString as BS
import           Data.Time.Clock.POSIX
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.List
import           Data.Maybe as Maybe
import           Database.SQLite.Simple as SQL
import           Database.SQLite.Simple.FromRow
import           Text.Printf
import           System.Random

import qualified Model

-- For Database structure or field questions, see the following:
-- https://github.com/ankidroid/Anki-Android/wiki/Database-Structure

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
    , cardOdid :: Int
    , cardFlags :: Int
    , cardData :: Text
    }
  deriving (Show, Generic, FromRow, ToRow)

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

readLastCard :: AnkiStore -> IO (Maybe ModelCard)
readLastCard s@AnkiStoreClosed{} =
  -- TODO: Throw / log error
  return Nothing
readLastCard s@AnkiStoreOpen{} = do
  rs <- query_ (conn s) "SELECT * FROM cards ORDER BY id DESC LIMIT 1" :: IO [ModelCard]
  case rs of
    []    -> return Nothing
    (n:_) -> return $ Just n

-- | Returns a base91-encoded 64bit random number
-- https://github.com/ankitects/anki/blob/9711044290dad315990f1281b148959068ec5de2/pylib/anki/utils.py#L128
genGuid64 :: IO Text
genGuid64 = do
  rInt <- randomRIO (1, 2^64 - 1) :: IO Int
  return . Base91.encode $ BS.pack [fromIntegral rInt]

storeNewNote :: AnkiStore -> Model.MCard -> IO()
storeNewNote s@AnkiStoreClosed{} _ = do
  -- TODO: Throw / log error
  putStrLn "[!] Anki store is closed!"
  return ()
storeNewNote s@AnkiStoreOpen{} newCard = do
  -- Retrieve an existing card, to copy attributes from.
  printf "[+] Storing card! %s\n" (Model.front newCard)

  lastNote_myb <- readLastNote s
  mapM
    (\lastNote -> do
      epochTime <- fmap round getPOSIXTime
      noteGuid <- genGuid64
      -- Store a standalone Note
      executeNamed
        (conn s)
        "INSERT INTO notes \
          \        (guid, mid, mod, usn, tags, flds, sfld, csum, flags, data) \
          \ VALUES (:guid, :mid, :mod, :usn, :tags, :flds, :sfld, :csum, :flags, :data)"
          ([ ":guid" := (noteGuid :: T.Text)
           , ":mid" := (noteMid lastNote :: Int) -- ModelID, copied from last Note.
           , ":mod" := (epochTime :: Int) -- Modification timestamp, epoch seconds
           , ":usn" := (-1 :: Int) -- Update sequence number for finding diffs when syncing.
           , ":tags" := ("" :: T.Text)
           , ":flds" := (T.pack $ printf "%s\US%s" (Model.front newCard) (Model.back newCard) :: T.Text)
           , ":sfld" := (T.pack $ printf "%s" (Model.front newCard) :: T.Text)
           , ":csum" := (123 :: Int) -- TODO
           , ":flags" := (0 :: Int)
           , ":data" := ("" :: T.Text)
           ])
      insertedNoteId <- lastInsertRowId (conn s)

      -- Read the Last Card stored in the DB, and base a new one off of it.
      whenJustM (readLastCard s)
        (\lastCard -> do
          -- Store a Card reference in a Deck that links to our new Note
          executeNamed
            (conn s)
            "INSERT INTO cards \
              \        (nid, did, ord, mod, usn, type, queue, due, ivl, factor, reps, lapses, left, odue, odid, flags, data) \
              \ VALUES (:nid, :did, :ord, :mod, :usn, :type, :queue, :due, :ivl, :factor, :reps, :lapses, :left, :odue, :odid, :flags, :data) "
              ([ ":nid" := insertedNoteId
               , ":did" := (cardDid lastCard :: Int) -- Copy the Deck ID from the last card
               , ":ord" := (0 :: Int)
               , ":mod" := (epochTime :: Int) -- Seconds since epoch
               , ":usn" := (-1 :: Int)
               -- Update sequence number. -1 indicates changes that need to be pushed to server.
               , ":type" := (0 :: Int)
               , ":queue" := (0 :: Int)
               , ":due" := (cardDue lastCard + 1 :: Int) -- Increment from last card
               , ":ivl" := (0 :: Int)
               , ":factor" := (0 :: Int)
               , ":reps" := (0 :: Int)
               , ":lapses" := (0 :: Int)
               , ":left" := (0 :: Int)
               , ":odue" := (0 :: Int)
               , ":odid" := (0 :: Int)
               , ":flags" := (0 :: Int)
               , ":data" := ("{}" :: T.Text)
               ])
        )
    )
    lastNote_myb

  return ()

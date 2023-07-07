module Model
  (MCard(..)) where

import Data.Text

data MCard =
  MCard
    { front :: Text
    , back :: Text
    }
  deriving (Show)

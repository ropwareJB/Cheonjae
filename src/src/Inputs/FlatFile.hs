module Inputs.FlatFile where

import Data.Text as T
import Data.Text.IO as T

readCards :: FilePath -> IO [Text]
readCards inputFile = do
  words <- readData inputFile
  return words

readData :: FilePath -> IO [Text]
readData fPath = do
  f <- T.readFile fPath
  return $ T.words f


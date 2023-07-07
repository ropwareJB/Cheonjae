{-# LANGUAGE DeriveDataTypeable #-}
module Args
  (Args(..)) where

import           Data.Text (Text)
import           System.Console.CmdArgs

data Args
  = ArgsDigest
    { input :: String
    , ankiStore :: String
    }
  deriving (Show, Data, Typeable)


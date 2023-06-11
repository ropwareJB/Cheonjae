module Tandem.Session where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString as BS
import           Data.ByteString.UTF8 as BSU
import qualified Text.Hex as T
import System.Environment

data Session = Session Text

getSession :: IO Session
getSession = do
  getEnv "TANDEM_SESSION" >>= return . Session . T.pack

decodeToken :: Text -> Maybe ByteString
decodeToken = T.decodeHex

encodeToBs :: Session -> ByteString
encodeToBs (Session t) = T.encodeUtf8 t


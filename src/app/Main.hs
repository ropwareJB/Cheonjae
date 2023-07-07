module Main (main) where

import System.Console.CmdArgs
import Args
import qualified Lib

digest :: Args
digest =
  ArgsDigest
    { input = ""
    , ankiStore = ""
    } &= name "digest"

mode :: Mode (CmdArgs Args)
mode = cmdArgsMode $ modes [digest]
  &= help "cheonjae"
  &= program "cheonjae"
  &= summary "\nv0.1.0, 2023, Joshua Brown"

main :: IO ()
main = do
  opts <- cmdArgsRun mode
  Lib.main opts

module Main where

import RIO
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Server (Env (..), app)
import System.Environment (lookupEnv)
import Prelude (read)

main :: IO ()
main = do
  ref <- newTVarIO []
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  staticFilePath <- fromMaybe "/var/www" <$> lookupEnv "STATIC_FILE_PATH"
  let env =
        Env
          { appPort = read pStr :: Int,
            staticDir = staticFilePath,
            scientistStore = ref
          }
  run (appPort env) $ app env

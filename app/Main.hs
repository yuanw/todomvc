module Main where

import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import RIO
import Server (Env (..), app)
import System.Environment (lookupEnv)
import Prelude (read)

main :: IO ()
main = do
  ref <- newTVarIO []
  port <- fmap (fromMaybe 8080 . join . fmap readMaybe) $ lookupEnv "PORT"
  staticFilePath <- fromMaybe "/var/www" <$> lookupEnv "STATIC_FILE_PATH"
  let env =
        Env
          { appPort = port,
            staticDir = staticFilePath,
            scientistStore = ref
          }
  run (appPort env) $ app env

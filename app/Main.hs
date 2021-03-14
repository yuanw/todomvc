module Main where

import ApiType (app)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  let port = read pStr :: Int
  run port app

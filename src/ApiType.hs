{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Aeson
import Data.Proxy ()
import GHC.Generics
import Servant

data Scientist = Scientist
  { sId :: Int,
    sName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Scientist

scientists :: [Scientist]
scientists =
  [ Scientist 1 "Isaac Newton",
    Scientist 2 "Albert Einstein",
    Scientist 3 "Gottfried Wilhelm Leibniz",
    Scientist 4 "Stephen Hawking",
    Scientist 5 "Pythagoras",
    Scientist 6 "Wernher Von Braun"
  ]

type API =
  "scientist" :> Get '[JSON] [Scientist]

--  :<|> "static" :> Raw

server :: Server API
server =
  pure scientists

-- :<|> serveDirectoryFileServer "./static"

myAPI :: Proxy API
myAPI = Proxy

app :: Application
app = serve myAPI server

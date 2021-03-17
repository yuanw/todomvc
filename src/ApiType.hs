{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE MultiParamTypeClasses #-}
module ApiType where

import Data.Aeson
import Data.ByteString.Lazy as Lazy
import Data.Proxy ()
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Servant
    ( Proxy(..),
      serve,
      serveDirectoryFileServer,
      type (:<|>)(..),
      JSON,
      MimeRender(..),
      Raw,
      type (:>),
      Get,
      Server,
      Application, Handler )
import Servant.API (Accept (..))
-- https://mmhaskell.com/blog/2020/3/23/serving-html-with-servant
newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

data HTML = HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- tell Servant how to render the newtype to html page, in this case simply unwrap it
instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

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
    :<|> Get '[HTML] RawHtml
    :<|> "static" :> Raw


renderIndex :: Html ()
renderIndex = html_ $ do
  head_ $ do
    title_ "User Page"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/styles.css"]
  body_ $ userBody
  where
    userBody = div_ [class_ "login-message"] $ do
        p_ "You aren't logged in!"
        br_ []
        a_ [href_ "/login"] "Please login"

indexHandler :: Handler RawHtml
indexHandler = return $ RawHtml (renderBS renderIndex)

server :: Server API
server =
  pure scientists
    :<|> indexHandler
    :<|> serveDirectoryFileServer "/var/www"

myAPI :: Proxy API
myAPI = Proxy

app :: Application
app = serve myAPI server

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
-- https://www.reddit.com/r/haskell/comments/5rvyy7/help_needed_to_understand_extendeddefaultrules/
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings   #-}


{-# LANGUAGE MultiParamTypeClasses #-}
module ApiType where

import Data.Aeson ( ToJSON )
import Data.ByteString.Lazy as Lazy ( ByteString )
import Data.Proxy ()
import GHC.Generics
import Lucid
    ( renderBS,
      src_,
      script_,
      body_,
      href_,
      type_,
      rel_,
      link_,
      title_,
      head_,
      html_,
      Html )
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

-- https://github.com/tryhaskell/tryhaskell/blob/d8b59e71d46cb890935f5c0c6c1d723cc9f78d99/src/TryHaskell.hs#L326-L419
renderIndex :: Html ()
renderIndex = html_ $ do
  head_ $ do
    title_ "Todomvc"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/styles.css"]
  body_ $ do 
    script_ [src_ "/static/main.js"] "" 

indexHandler :: Handler RawHtml
indexHandler = return $ RawHtml (renderBS renderIndex)

server :: String -> Server API
server path =
  pure scientists
    :<|> indexHandler
    :<|> serveDirectoryFileServer path

myAPI :: Proxy API
myAPI = Proxy

app :: String -> Application
app filePath = serve myAPI (server filePath)

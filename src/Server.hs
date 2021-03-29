-- https://www.reddit.com/r/haskell/comments/5rvyy7/help_needed_to_understand_extendeddefaultrules/
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Server where

import Api (API, Scientist (..))
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Lazy as Lazy (ByteString)
import Data.Proxy ()
import Lucid
  ( Html,
    body_,
    head_,
    href_,
    html_,
    link_,
    rel_,
    renderBS,
    script_,
    src_,
    title_,
    type_,
  )
import Network.HTTP.Media ((//), (/:))
import Servant hiding (serveDirectoryWebApp)
import Servant.RawM.Server (serveDirectoryWebApp)

data Env = Env
  { appPort :: Int,
    staticDir :: FilePath,
    scientistStore :: !(TVar [Scientist])
  }

-- https://mmhaskell.com/blog/2020/3/23/serving-html-with-servant
newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

data HTML = HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- tell Servant how to render the newtype to html page, in this case simply unwrap it
instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

scientists :: [Scientist]
scientists =
  [ Scientist 1 "Isaac Newton",
    Scientist 2 "Albert Einstein",
    Scientist 3 "Gottfried Wilhelm Leibniz",
    Scientist 4 "Stephen Hawking",
    Scientist 5 "Pythagoras",
    Scientist 6 "Wernher Von Braun"
  ]

-- https://github.com/tryhaskell/tryhaskell/blob/d8b59e71d46cb890935f5c0c6c1d723cc9f78d99/src/TryHaskell.hs#L326-L419
renderIndex :: Html ()
renderIndex = html_ $ do
  head_ $ do
    title_ "Todomvc"
    link_ [rel_ "icon", href_ "/static/favicon.ico"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/styles.css"]
  body_ $ do
    script_ [src_ "/static/main.js"] ""

indexHandler :: ReaderT Env IO RawHtml
indexHandler = return $ RawHtml (renderBS renderIndex)

--scientistsHandler :: ReaderT Env IO [Scientist]

rawEndpoint :: ReaderT Env IO Application
rawEndpoint = do
  env <- ask
  serveDirectoryWebApp (staticDir env)

-- https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html
serverRoot :: ServerT API (ReaderT Env IO)
serverRoot = pure scientists :<|> rawEndpoint

myAPI :: Proxy API
myAPI = Proxy

app :: Env -> Application
app env = serve myAPI apiServer
  where
    apiServer :: Server API
    apiServer = hoistServer myAPI transformation serverRoot

    transformation :: ReaderT Env IO a -> Handler a
    transformation readerT = liftIO $ runReaderT readerT env

{-# LANGUAGE ExplicitNamespaces #-}
-- https://www.reddit.com/r/haskell/comments/5rvyy7/help_needed_to_understand_extendeddefaultrules/
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Api                            ( API
                                                , RawHtml(..)
                                                , Scientist(..)
                                                )
import           Control.Concurrent.STM         ( TVar )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , ReaderT(runReaderT)
                                                )
import           Data.Proxy                     ( )
import           Lucid                          ( Html
                                                , body_
                                                , head_
                                                , href_
                                                , html_
                                                , link_
                                                , rel_
                                                , renderBS
                                                , script_
                                                , src_
                                                , title_
                                                , type_
                                                )
import           Servant                        ( Application
                                                , Handler
                                                , HasServer(ServerT)
                                                , Proxy(..)
                                                , Server
                                                , hoistServer
                                                , serve
                                                )
import           Servant.API                    ( type (:<|>)((:<|>)) )
import           Servant.RawM.Server            ( serveDirectoryWebApp )

data Env = Env
  { appPort        :: Int
  , staticDir      :: FilePath
  , scientistStore :: !(TVar [Scientist])
  }

scientists :: [Scientist]
scientists =
  [ Scientist
    1
    "Isaac Newton"
    "https://upload.wikimedia.org/wikipedia/commons/3/39/GodfreyKneller-IsaacNewton-1689.jpg"
  , Scientist
    2
    "Albert Einstein"
    "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3e/Einstein_1921_by_F_Schmutzer_-_restoration.jpg/440px-Einstein_1921_by_F_Schmutzer_-_restoration.jpg"
  , Scientist
    3
    "Gottfried Wilhelm Leibniz"
    "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8d/Christoph_Bernhard_Francke_-_Bildnis_des_Philosophen_Leibniz_%28ca._1695%29.jpg/440px-Christoph_Bernhard_Francke_-_Bildnis_des_Philosophen_Leibniz_%28ca._1695%29.jpg"
  , Scientist
    4
    "Stephen Hawking"
    "https://upload.wikimedia.org/wikipedia/commons/e/eb/Stephen_Hawking.StarChild.jpg"
  , Scientist
    5
    "Pythagoras"
    "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Pythagoras_in_the_Roman_Forum%2C_Colosseum.jpg/440px-Pythagoras_in_the_Roman_Forum%2C_Colosseum.jpg"
  , Scientist
    6
    "Wernher Von Braun"
    "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Wernher_von_Braun_1960.jpg/440px-Wernher_von_Braun_1960.jpg"
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
serverRoot = pure scientists :<|> indexHandler :<|> rawEndpoint

myAPI :: Proxy API
myAPI = Proxy

app :: Env -> Application
app env = serve myAPI apiServer
 where
  apiServer :: Server API
  apiServer = hoistServer myAPI transformation serverRoot

  transformation :: ReaderT Env IO a -> Handler a
  transformation readerT = liftIO $ runReaderT readerT env

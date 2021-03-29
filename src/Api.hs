{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson
import Data.ByteString.Lazy as Lazy (ByteString)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), Get, JSON, MimeRender (..), (:<|>), (:>))
import Servant.RawM (RawM)

data Scientist = Scientist
  { sId :: Int,
    sName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Scientist

-- https://mmhaskell.com/blog/2020/3/23/serving-html-with-servant
newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

data HTML = HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- tell Servant how to render the newtype to html page, in this case simply unwrap it
instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type API = GetScientists :<|> Get '[HTML] RawHtml :<|> RawEndpoint

type GetScientists = "scientist" :> Get '[JSON] [Scientist]

type RawEndpoint = "static" :> RawM

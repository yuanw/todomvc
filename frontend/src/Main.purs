module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import CSS.Flexbox (AlignContentValue, JustifyContentValue)
import CSS.Geometry (width)
import CSS.Property (Value)
import CSS.Size (rem)
import CSS.String (fromString)
import Data.Array (head, index)
import Data.Maybe (Maybe(..), isJust)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event


data Action = Initialze | Increment | Decrement
type Scientist  = {
   sName :: String
 , sPhotoUrl :: String
 , sId :: Int
}


type State =
  { counter :: Int
  , loading :: Boolean
  , scientists :: Array Scientist
  , currentScientist :: Maybe Scientist
  }


class SpaceEvenly a where
  spaceEvenly :: a
instance spaceEvenlyValue :: SpaceEvenly Value where
  spaceEvenly = fromString "space-evenly"
instance spaceEvenlyAlignContentValue :: SpaceEvenly AlignContentValue where
  spaceEvenly = fromString "space-evenly"
instance spaceEvenlyJustifyContentValue :: SpaceEvenly JustifyContentValue where
  spaceEvenly = fromString "space-evenly"


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialze}
    }
  where
  initialState :: forall input'. input' -> State
  initialState _ = {counter: 0, loading: false, scientists: [], currentScientist : Nothing}

  render state =
    HH.div_ [
      HH.div_ [
        HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.text (show state.counter)
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
        ]
      , HH.h2_ [ HH.text "The Great Scientists" ]
    , HH.p_
        [ HH.text $ if state.loading then "Working..." else "" ]
    , HH.div_
        case state.currentScientist of
          Nothing -> []
          Just s ->
            [ HH.div_ [HH.img [HP.src s.sPhotoUrl]]
            , HH.div_ [HH.text s.sName ]]
      ]

handleAction :: forall output m . MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    Initialze -> do
      H.modify_ _ { loading = true }
      res <- H.liftAff $ AX.request (AX.defaultRequest { url = "/scientist", method = Left GET, responseFormat = ResponseFormat.json })
      case res of
        Left err -> do
          H.liftEffect $ log $ "Get /scientist error" <> AX.printError err
          H.modify_ _ {loading = false}
        Right response -> do
          case (decodeJson response.body :: Either JsonDecodeError (Array Scientist) ) of
            Left e -> do
              log $ "can't parse json: " <> show e
              H.modify_ _ { loading = false}
            Right sc -> do
              log "loaded"
              H.modify_ _ {loading = false, scientists = sc,  currentScientist = head sc }

    Decrement ->
      H.modify_ \s -> s {counter = s.counter - 1,
                         currentScientist = index s.scientists (s.counter - 1) }

    Increment ->
      H.modify_ \s -> s {counter = s.counter + 1,
                         currentScientist = index s.scientists (s.counter + 1) }

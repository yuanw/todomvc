module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import CSS.Display (display, flex)
import CSS.Flexbox (AlignContentValue, JustifyContentValue, flexDirection, justifyContent, row)
import CSS.Geometry (width)
import CSS.Property (Value)
import CSS.Size (rem)
import CSS.String (fromString)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event


data Action = Increment | Decrement | MakeRequest Event

type Scientist  = {
  name :: String
}


type State =
  { counter :: Int
  , loading :: Boolean
  , scientists :: Array Scientist
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
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: forall input'. input' -> State
  initialState _ = {counter: 0, loading: false, scientists: []}

  render state =
    HH.div_ [
      HH.div_ [
        HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.text (show state.counter)
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
        ]
      ,

      HH.form
      [ HE.onSubmit \ev -> MakeRequest ev ]

      [ HH.h2_ [ HH.text "Look up Scientists" ]
      , HH.button
        [ HP.disabled state.loading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Fetch scientists" ]
    , HH.p_
        [ HH.text $ if state.loading then "Working..." else "" ]
     ]
      ]

handleAction :: forall output m . MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    Decrement ->
      H.modify_ \s -> s {counter = s.counter - 1 }

    Increment ->
      H.modify_ \s -> s {counter = s.counter + 1 }

    MakeRequest event -> do
      H.liftEffect $ Event.preventDefault event
      H.modify_ _ { loading = true }
      response <- H.liftAff $ AX.request (AX.defaultRequest { url = "/scientist", method = Left GET, responseFormat = ResponseFormat.json })
      case (decodeJson =<< response :: Either JsonDecodeError (Array Scientist) ) of
        Left e -> H.modify_ _ { loading = false}
        Right sc -> H.modify_ _ {loading = false, scientists = sc}

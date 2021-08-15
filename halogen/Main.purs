module Main where

import Prelude

import CSS.Display (display, flex)
import CSS.Flexbox (AlignContentValue, JustifyContentValue, flexDirection, justifyContent, row )
import CSS.Geometry (width)
import CSS.Property (Value)
import CSS.Size (rem)
import CSS.String (fromString)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

data Action = Increment | Decrement
type State = Int

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

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: forall input'. input' -> State
  initialState _ = 0

  render state =
    HH.div
      [
        HC.style $
           display flex
        <> flexDirection row
        <> justifyContent spaceEvenly
        <> width (rem 6.0)
      ]
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.text (show state)
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Decrement ->
      H.modify_ \state -> state - 1

    Increment ->
      H.modify_ \state -> state + 1


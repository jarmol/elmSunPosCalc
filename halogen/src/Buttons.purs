module Example.Driver.IO.Main where
import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Increment | Inc10 | Clear | Decrement

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.h1 [] [ HH.text "Buttons & Counter"]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "Increment" ]
      , HH.button [ HE.onClick \_ -> Inc10 ] [ HH.text "Inc 10" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "Decrement" ]
      , HH.button [ HE.onClick \_ -> Clear ] [ HH.text "Clear" ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Inc10     -> H.modify_ \state -> state + 10
    Clear     -> H.modify_ \state -> 0 * state
    Decrement -> H.modify_ \state -> state - 1

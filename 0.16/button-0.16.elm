import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

type alias Counter = Int
type Action = Increment | NoOp
styles = [("text-align", "center"), ("height", "500px"), ("width", "1400px"), ("font-size", "xx-large"), ("margin-top", "300px")]

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

view : Counter -> Html
view counter =
  div [style styles] [text <| toString counter, button [onClick actions.address Increment] [text "Increment"]]

counter : Signal Counter
counter = Signal.foldp updateCounter 0 actions.signal

updateCounter : Action -> Counter -> Counter
updateCounter action counter =
  case action of
    Increment -> counter + 1
    _ -> counter

main : Signal Html
main = Signal.map view counter

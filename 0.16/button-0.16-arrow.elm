import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Automaton exposing (..)

type alias Counter = Int
type Action = Increment | NoOp
styles = [("text-align", "center"), ("height", "500px"), ("width", "1400px"), ("font-size", "xx-large"), ("margin-top", "300px")]

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

view : Counter -> Html
view counter =
  div [style styles] [text <| toString counter, button [onClick actions.address Increment] [text "Increment"]]

counterArrow : Automaton Action Counter
counterArrow = Automaton.loop 0 <| pure updateCounter

updateCounter : (Action, Counter) -> (Counter, Counter)
updateCounter (action, counter) =
  case action of
    Increment -> dup <| counter + 1
    _ -> dup <| counter

dup : a -> (a, a)
dup a = (a, a)

initView : Html
initView = view 0

main : Signal Html
main = run (counterArrow >>> pure view) initView actions.signal

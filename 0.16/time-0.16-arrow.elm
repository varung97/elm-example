import Graphics.Element exposing (..)
import Time
import Text
import Automaton exposing (..)

type Action = Tick
type alias Counter = Int

time : Signal Time.Time
time = Time.every Time.second

actions : Signal Action
actions = Signal.map (always Tick) time

state : Automaton Action Counter
state = Automaton.loop 0 <| pure updateCounter

updateCounter : (Action, Counter) -> (Counter, Counter)
updateCounter (action, counter) =
  case action of
    Tick -> dup <| counter + 1

dup : a -> (a, a)
dup a = (a, a)

main : Signal Element
main = Automaton.run (state >>> pure view) (view 0) actions

view : Counter -> Element
view counter =
  container 1400 500 middle <| centered <| Text.height 40 <| Text.fromString <| toString counter

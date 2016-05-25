import Graphics.Element exposing (..)
import Time
import Text

type Action = Tick
type alias Counter = Int

time : Signal Time.Time
time = Time.every Time.second

actions : Signal Action
actions = Signal.map (always Tick) time

state : Signal Counter
state = Signal.foldp updateCounter 0 actions

updateCounter : Action -> Counter -> Counter
updateCounter action counter =
  case action of
    Tick -> counter + 1

main : Signal Element
main = Signal.map view state

view : Counter -> Element
view counter =
  container 1400 500 middle <| centered <| Text.height 40 <| Text.fromString <| toString counter

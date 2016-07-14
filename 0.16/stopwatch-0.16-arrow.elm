import Graphics.Element exposing (..)
import Time
import Text
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Automaton exposing (..)

type Action = Tick | StartStop | LapReset | NoOp
type alias Model =
  {
    time: Int,
    frozenTime: Int,
    running: Bool,
    frozen: Bool
  }

initModel : Model
initModel =
  {
    time = 0,
    frozenTime = 0,
    running = True,
    frozen = False
  }

time : Signal Action
time = Signal.map (always Tick) <| Time.every (Time.millisecond * 10)

actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox NoOp

actions : Signal Action
actions = Signal.merge actionMailbox.signal time

stateArrow : Automaton Action Model
stateArrow = Automaton.loop initModel <| pure update

update : (Action, Model) -> (Model, Model)
update (action, model) =
  let newModel = case action of
    Tick ->
      let (time, frozenTime) =
        if (model.running && not model.frozen) then
          (model.time + 1, model.time + 1)
        else if (model.running && model.frozen) then
          (model.time + 1, model.frozenTime)
        else if (not model.running && model.frozen) then
          (0, 0)
        else
          (model.time, model.time)
      in
        {model | time = time, frozenTime = frozenTime}
    StartStop ->
      {model | running = not model.running}
    LapReset ->
      {model | frozen = not model.frozen}
    NoOp ->
      model
  in
    dup newModel

dup : a -> (a, a)
dup a = (a, a)

main : Signal Html
main = run (stateArrow >>> pure view) initView actions

view : Model -> Html
view {time, frozenTime, frozen} =
  let
    display =
      if frozen then
        frozenTime
      else
        time
    displayStr = toString (display // 100) ++ "." ++ toString (display % 100)
    txt = fromElement <| container 1000 500 middle <| centered <| Text.height 40 <| Text.fromString displayStr
    but1 = button [onClick actionMailbox.address StartStop] [text "Start/Stop"]
    but2 = button [onClick actionMailbox.address LapReset] [text "Lap/Reset"]
  in
    div [] [but1, but2, txt]

initView : Html
initView = view initModel

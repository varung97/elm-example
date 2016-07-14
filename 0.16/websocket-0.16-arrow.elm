import Graphics.Element exposing (..)
import Task exposing (Task, andThen)
import String
import Time
import SocketIO
import Text
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Automaton exposing (..)

eventName = "echo"
type Action = Send | NoOp

-- Fails with type x and succeeds with type SocketIO.Socket
socket : Task x SocketIO.Socket
socket = SocketIO.io "http://localhost:8001" SocketIO.defaultOptions

-- port initial : Task x ()
-- port initial = socket `andThen` SocketIO.emit eventName "Hi"

-- Creates a signal mailbox, with default value null, to which the reponses will
-- be sent
received : Signal.Mailbox String
received = Signal.mailbox "null"

-- set up the receiving of data
-- Server emits on same event name (contrived) and this is sent to the mailbox
port responses : Task x ()
port responses = socket `andThen` SocketIO.on eventName received.address

-- Create a signal mailbox with default value NoOp which receives the commands
-- to send message
sendMessageMailbox : Signal.Mailbox Action
sendMessageMailbox = Signal.mailbox NoOp

-- convert the action to a string to be sent
action : Action -> String
action act =
  case act of
    Send -> "Echo"
    NoOp -> "null"

-- convert the actions to a task so that message is sent to the websocket
port sendMessage : Signal (Task x ())
port sendMessage = Signal.map (\act -> socket `andThen` SocketIO.emit eventName (action act)) sendMessageMailbox.signal

view : String -> Html
view str =
  let
    txt = fromElement <| container 1000 500 middle <| centered <| Text.height 40 <| Text.fromString <| str
    -- onClick sends an action "Send" to the mailbox specified
    but = button [onClick sendMessageMailbox.address Send] [text "Send Message"]
  in
    div [] [txt, but]

main : Signal Html
main = Signal.map view received.signal

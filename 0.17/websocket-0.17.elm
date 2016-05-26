import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket

type alias Model = String
type Msg = NewMessage String | Send

styles = [("text-align", "center"), ("height", "400px"), ("width", "1400px"), ("font-size", "xx-large"), ("margin-top", "300px")]
websocket = "ws://echo.websocket.org"

main : Program Never
main =
  App.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

init : (Model, Cmd Msg)
init =
  ("No message yet", Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen websocket NewMessage

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewMessage message -> (message, Cmd.none)
    Send -> (model, WebSocket.send websocket "Echo")

view : Model -> Html Msg
view model =
  div []
    [
      button [onClick Send] [text "Click Me!"],
      p [style styles] [text model]
    ]

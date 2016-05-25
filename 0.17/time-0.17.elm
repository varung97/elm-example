import Time exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

type alias Model = Int
type Msg = Tick Time
styles = [("text-align", "center"), ("height", "500px"), ("width", "1400px"), ("font-size", "xx-large"), ("margin-top", "300px")]

main : Program Never
main =
  App.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

init : (Model, Cmd Msg)
init = (0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ -> (model + 1, Cmd.none)

view : Model -> Html Msg
view model =
  p [style styles] [text <| toString model]

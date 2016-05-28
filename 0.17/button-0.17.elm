import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model = Int
type Msg = Increment
styles = [("text-align", "center"), ("height", "500px"), ("width", "1400px"), ("font-size", "xx-large"), ("margin-top", "300px")]

main : Program Never
main =
  App.program
    { init = init
    , subscriptions = (\_ -> Sub.none)
    , update = update
    , view = view
    }

init : (Model, Cmd Msg)
init = (0, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment -> (model + 1, Cmd.none)

view : Model -> Html Msg
view model =
  div [style styles] [text <| toString model, button [onClick Increment] [text "Increment"]]

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Signal exposing (Address)
import StartApp
import Task exposing (Task)


type alias Model =
  { topic : String
  , gifUrl : Result String String
  }


type Action
  = MorePlease
  | FetchSucceed String
  | FetchFail Http.Error


main : Signal Html
main =
  app.html


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }


init : (Model, Effects Action)
init =
  ( Model "cats" (Err "loading...")
  , getRandomGif "cats"
  )


view : Address Action -> Model -> Html
view address model =
  div []
    [ h2 [] [text model.topic]
    , button [ onClick address MorePlease ] [ text "More Please!" ]
    , br [] []
    , viewGif model
    ]


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    MorePlease ->
      (model, getRandomGif model.topic)
    FetchSucceed newUrl ->
      ({ model | gifUrl = Ok newUrl }, Effects.none)
    FetchFail error ->
      ({ model | gifUrl = Err (extractErrorMessage error) }, Effects.none)


port tasks : Signal (Task Effects.Never ())
port tasks = app.tasks


viewGif : Model -> Html
viewGif model =
  case model.gifUrl of
    Ok gifUrl ->
      img [src gifUrl] []
    Err error ->
      text error


getRandomGif : String -> Effects Action
getRandomGif topic =
  let
    url =
      "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Effects.task <|
      Task.map FetchSucceed (Http.get decodeGifUrl url)
        `Task.onError` (FetchFail >> Task.succeed)


decodeGifUrl : Json.Decoder String
decodeGifUrl =
  Json.at ["data", "image_url"] Json.string


extractErrorMessage : Http.Error -> String
extractErrorMessage error =
  case error of
    Http.Timeout ->
      "Timeout"
    Http.NetworkError ->
      "Network error"
    Http.UnexpectedPayload action ->
      "Unexpected payload: " ++ action
    Http.BadResponse code action ->
      "Bad response: " ++ toString code ++ action

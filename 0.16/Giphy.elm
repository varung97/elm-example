import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Signal exposing (Address, Mailbox)
import Task exposing (Task)


type alias Model =
  { topic : String
  , gifUrl : Result String String
  }


type Action
  = MorePlease
  | FetchSucceed String
  | FetchFail Http.Error
  | NoOp


type Never =
  Never


type alias State =
  (Model, Task Never Action)


main : Signal Html
main =
  Signal.map (view messages.address << fst) states


init : State
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


update : Action -> Model -> State
update action model =
  case action of
    MorePlease ->
      (model, getRandomGif model.topic)
    FetchSucceed newUrl ->
      ({ model | gifUrl = Ok newUrl }, Task.succeed NoOp)
    FetchFail error ->
      ({ model | gifUrl = Err (extractErrorMessage error) }, Task.succeed NoOp)
    NoOp ->
      (model, Task.succeed NoOp)


port tasks : Signal (Task Never ())
port tasks =
  Signal.map
    (\(_, task) -> task `Task.andThen` Signal.send messages.address)
    states


messages : Mailbox Action
messages = Signal.mailbox NoOp


updateState : Action -> State -> State
updateState action (model, _) =
  update action model


states : Signal State
states =
  Signal.foldp updateState init messages.signal


viewGif : Model -> Html
viewGif model =
  case model.gifUrl of
    Ok gifUrl ->
      img [src gifUrl] []
    Err error ->
      text error


getRandomGif : String -> Task Never Action
getRandomGif topic =
  let
    url =
      "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
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

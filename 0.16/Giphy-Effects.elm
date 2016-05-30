import Effects exposing (Effects)
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


type alias State =
  (Model, Effects Action)


main : Signal Html
main =
  Signal.map (view adr << fst) states


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
      ({ model | gifUrl = Ok newUrl }, Effects.none)
    FetchFail error ->
      ({ model | gifUrl = Err (extractErrorMessage error) }, Effects.none)


port tasks : Signal (Task Effects.Never ())
port tasks =
  Signal.map
    (\(_, effect) -> Effects.toTask messages.address effect)
    states


messages : Mailbox (List Action)
messages = Signal.mailbox []


adr : Address Action
adr = Signal.forwardTo messages.address (\x -> [x])


updateState : List Action -> State -> State
updateState actions (model, _) =
  let
    updateStep action (oldModel, accumulatedEffects) =
      let
        (newModel, additionalEffects) = update action oldModel
      in
        (newModel, Effects.batch [accumulatedEffects, additionalEffects])
  in
    List.foldl updateStep (model, Effects.none) actions


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


getRandomGif : String -> Effects Action
getRandomGif topic =
  let
    url =
      "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.map FetchSucceed (Http.get decodeGifUrl url)
      `Task.onError` (FetchFail >> Task.succeed)
      |> Effects.task


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

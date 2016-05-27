import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task


type alias Model =
  { topic : String
  , gifUrl : Result String String
  }


type Msg
  = MorePlease
  | FetchSucceed String
  | FetchFail Http.Error


main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init =
  ( Model "cats" (Err "")
  , getRandomGif "cats"
  )


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , viewGif model
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomGif model.topic)
    FetchSucceed newUrl ->
      ({ model | gifUrl = Ok newUrl }, Cmd.none)
    FetchFail error ->
      ({ model | gifUrl = Err (extractErrorMessage error) }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


viewGif : Model -> Html Msg
viewGif model =
  case model.gifUrl of
    Ok gifUrl ->
      img [src gifUrl] []
    Err error ->
      text error


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)


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
    Http.UnexpectedPayload msg ->
      "Unexpected payload: " ++ msg
    Http.BadResponse code msg ->
      "Bad response: " ++ toString code ++ msg

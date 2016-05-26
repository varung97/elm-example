module StartAppSimple where

import Debug
import Html exposing (Html)
import Signal exposing (Address, Mailbox)


type alias Config model action =
  { model : model
  , view : Address action -> model -> Html
  , update : action -> model -> model
  }


-- Comment out type annotations because user-defined type variables are not
-- shared between annotations.
-- https://github.com/elm-lang/elm-compiler/blob/0.16.0/hints/type-annotations.md
start : Config model action -> Signal Html
start config =
  let
    -- Create a mailbox where actions are sent to.
    -- Wrap action in Maybe so that we can create a mailbox with a default
    -- value for mailbox.signal without knowing what action is.
    -- actions : Mailbox (Maybe action)
    actions =
      Signal.mailbox Nothing

    -- Address of the right type.
    -- Wrap actions in Maybe and forward them to actions.address.
    -- address : Address action
    address =
      Signal.forwardTo actions.address Just

    -- update : (Maybe action) -> model -> model
    update maybeAction model =
      case maybeAction of
        Just action ->
          config.update action model

        Nothing ->
          Debug.crash "This should never happen."

    -- model : Signal model
    model =
      Signal.foldp update config.model actions.signal
  in
    Signal.map (config.view address) model


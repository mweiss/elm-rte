module HandleCompositionEnd exposing (..)

import DocumentUtils exposing (insertAtSelection)
import Model exposing (Document, Msg)


handleCompositionEnd : String -> Document -> ( Document, Cmd Msg )
handleCompositionEnd data model =
    let
        newDoc =
            insertAtSelection data model
    in
    ( { newDoc | isComposing = False }, Cmd.none )

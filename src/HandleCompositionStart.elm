module HandleCompositionStart exposing (..)

import Model exposing (Document, Msg)


handleCompositionStart : Document -> ( Document, Cmd Msg )
handleCompositionStart model =
    ( { model | isComposing = True }, Cmd.none )

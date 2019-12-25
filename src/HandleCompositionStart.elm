module HandleCompositionStart exposing (..)

import Model exposing (Document, Msg)



{-
   Sets the isComposing flag to true, which is useful for events which need to know if an IME
   is present or not.
-}


handleCompositionStart : Document -> ( Document, Cmd Msg )
handleCompositionStart model =
    ( { model | isComposing = True }, Cmd.none )

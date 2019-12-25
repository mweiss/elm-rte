module HandleCompositionEnd exposing (..)

{-
   This module holds code relating to handling the compositionEnd function.
-}

import DocumentUtils exposing (insertAtSelection)
import Model exposing (Document, Msg)



{-
   Inserts the composed code at the document's selection.  Also sets the composing flag to false.
-}


handleCompositionEnd : String -> Document -> ( Document, Cmd Msg )
handleCompositionEnd data model =
    let
        newDoc =
            insertAtSelection data model
    in
    ( { newDoc | isComposing = False }, Cmd.none )

module HandleCompositionEnd exposing (..)

{-
   This module holds code relating to handling the compositionEnd function.
-}

import DocumentUtils exposing (insertAtSelection)
import Model exposing (CompositionEnd, Document, Msg)



{-
   Inserts the composed code at the document's selection.  Also sets the composing flag to false.
-}


handleCompositionEnd : CompositionEnd -> Document -> ( Document, Cmd Msg )
handleCompositionEnd compositionEnd model =
    --let
    -- newDoc =
    --    insertAtSelection compositionEnd.data model
    -- in
    ( { model | isComposing = False }, Cmd.none )

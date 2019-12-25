module HandleCut exposing (..)

import DocumentUtils
import Model exposing (Document, Msg)



{-
   Handles cut logic by removing the selection from the document, and forcing a rerender (this is
   necessary because the DOM will be natively updated, which could cause the VirtualDOM to get out
   of sync and throw many exceptions.
-}


handleCut : Document -> Document
handleCut document =
    case document.selection of
        Nothing ->
            document

        Just selection ->
            let
                newDocument =
                    DocumentUtils.removeSelected selection document
            in
            { newDocument | renderCount = newDocument.renderCount + 1 }

module HandleCut exposing (..)

import DocumentUtils
import Model exposing (Document, Msg)


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

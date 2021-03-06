module HandlePasteWithData exposing (..)

{-
   This module deals with the custom pastewithdata event which contains the text and html data of the clipboard,
   which we then parse and insert into the document if applicable.  Currently, only text parsing has been implemented.
-}

import DocumentUtils
import Model exposing (..)


handlePasteWithData : PasteWithData -> Document -> Document
handlePasteWithData pasteWithData document =
    case document.selection of
        Nothing ->
            document

        Just selection ->
            let
                ( newNodes, newDocument ) =
                    DocumentUtils.addIdsToDocumentNodes (textToDocumentNodes pasteWithData.text) document

                documentWithInsertedText =
                    DocumentUtils.replaceSelected newNodes selection newDocument
            in
            documentWithInsertedText


textToDocumentNodes : String -> List DocumentNode
textToDocumentNodes text =
    List.map
        (\line ->
            { id = ""
            , characterMetadata = List.repeat (String.length line) emptyCharacterMetadata
            , text = line
            , nodeType = "div"
            }
        )
        (String.split
            "\n"
            text
        )

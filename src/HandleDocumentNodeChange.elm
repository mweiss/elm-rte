module HandleDocumentNodeChange exposing (..)

import Json.Decode as D
import List.Extra
import Model exposing (..)


decodeDocumentNodeChange : D.Decoder Msg
decodeDocumentNodeChange =
    D.map OnDocumentNodeChange
        (D.map2 DocumentNodeChange
            (D.at [ "detail", "node" ] D.string)
            (D.at [ "detail", "text" ] D.string)
        )



-- TODO, this will clear style data, but I think it can be done in a way that's somewhat intelligent
-- about preserving the style it can


handleDocumentNodeChange : DocumentNodeChange -> Document -> Document
handleDocumentNodeChange documentNodeChange document =
    let
        maybeNode =
            List.Extra.find (\documentNode -> documentNodeChange.node == documentNode.id) document.nodes
    in
    case maybeNode of
        Nothing ->
            document

        Just node ->
            if node.text == documentNodeChange.text then
                document

            else
                { document
                    | renderCount = document.renderCount + 1
                    , nodes =
                        List.map
                            (\n ->
                                if n.id == node.id then
                                    { n | text = documentNodeChange.text, characterMetadata = List.repeat (String.length documentNodeChange.text) document.currentStyles }

                                else
                                    n
                            )
                            document.nodes
                }

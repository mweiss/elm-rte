module EditorNodeToHtml exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)


editorNodeToHtml : EditorNode -> Html msg
editorNodeToHtml editorNode =
    case editorNode of
        BlockEditorNode node ->
            Html.div
                [ id node.id, attribute "data-document-node-id" node.id ]
                (List.map editorNodeToHtml node.childNodes)

        LeafEditorNode node ->
            let
                leafEditorNodeId =
                    String.concat [ node.id, "-", String.fromInt node.offset ]
            in
            Html.span
                [ id (String.concat [ leafEditorNodeId ])
                , attribute "data-document-node-offset" (String.fromInt node.offset)
                , attribute "data-document-node-offset-id" leafEditorNodeId
                ]
                [ Html.text node.text ]

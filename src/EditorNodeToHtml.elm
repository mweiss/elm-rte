module EditorNodeToHtml exposing (..)

import Dict exposing (Dict, toList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)


editorNodeStyles : Dict String String -> List (Attribute msg)
editorNodeStyles styles =
    List.map (\( k, v ) -> style k v) (toList styles)


editorNodeToHtml : EditorNode -> Html msg
editorNodeToHtml editorNode =
    case editorNode of
        BlockEditorNode node ->
            Html.div
                [ id node.id, attribute "data-document-node-id" node.id ]
                (if List.isEmpty node.childNodes then
                    [ Html.span [ attribute "data-document-node-offset" "0" ] [ Html.text "" ]
                    , Html.br [] []
                    ]

                 else
                    List.map editorNodeToHtml node.childNodes
                )

        LeafEditorNode node ->
            Html.span
                ([ attribute "data-document-node-offset" (String.fromInt node.offset)
                 , style "white-space" "pre-wrap"
                 ]
                    ++ editorNodeStyles node.styles
                )
                [ Html.text node.text ]

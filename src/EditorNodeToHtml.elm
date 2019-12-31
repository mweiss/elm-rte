module EditorNodeToHtml exposing (..)

{-
   This module holds functions which turns the intermediary format EditorNode into HTML.
-}

import Dict exposing (Dict, toList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Set


editorNodeStyles : Dict String String -> List (Attribute msg)
editorNodeStyles styles =
    List.map (\( k, v ) -> style k v) (toList styles)



-- TODO: Use an ordered set... I can't seem to find one that's up to date in Elm though


characterMetadataToString : CharacterMetadata -> String
characterMetadataToString cm =
    String.join "," (List.sort (Set.toList cm.styles))


editorNodeToHtml : EditorNode -> Html msg
editorNodeToHtml editorNode =
    case editorNode of
        BlockEditorNode node ->
            Html.node
                node.nodeType
                [ attribute "data-document-node-id" node.id
                , attribute "data-document-node-type" node.documentNodeType
                ]
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
                 , attribute "data-character-metadata" (characterMetadataToString node.characterMetadata)
                 , style "white-space" "pre-wrap"
                 ]
                    ++ editorNodeStyles node.styles
                )
                [ Html.text node.text ]

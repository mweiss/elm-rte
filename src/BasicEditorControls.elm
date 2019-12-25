module BasicEditorControls exposing (..)

import DocumentUtils
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model exposing (Document, Msg(..))
import Set


editorView : Document -> Html Msg
editorView model =
    div [] [ editorPanel model ]


isSelectedBlock : String -> Document -> Bool
isSelectedBlock action document =
    case document.selection of
        Nothing ->
            False

        Just selection ->
            let
                ( _, selected, _ ) =
                    DocumentUtils.getSelectionBlocks selection document.nodes
            in
            List.length selected > 0 && not (List.any (\x -> x.nodeType /= action) selected)


isCurrentStyle : String -> Document -> Bool
isCurrentStyle action document =
    Set.member action document.currentStyles.styles


createButton : Document -> Bool -> String -> Html Msg
createButton document isBlockAction action =
    let
        selected =
            if isBlockAction then
                isSelectedBlock action document

            else
                isCurrentStyle action document
    in
    span
        ([ onClick (OnButtonPress action), class "rte-button" ]
            ++ (if selected then
                    [ class "rte-button-selected" ]

                else
                    []
               )
        )
        [ text action ]


editorPanel : Document -> Html Msg
editorPanel document =
    div []
        [ div [ class "rte-controls" ]
            (List.map
                (createButton document True)
                [ "H1", "H2", "H3", "H4", "H5", "H6", "Blockquote" ]
            )
        , div [ class "rte-controls" ]
            (List.map
                (createButton document False)
                [ "Bold", "Italic", "Underline", "Monospace" ]
            )
        ]

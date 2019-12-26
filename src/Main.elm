port module Main exposing (updateSelection)

{- | The main module for the Elm rich text editor.  This implements a prototype contenteditable
   rich text editor with most of the logic done in Elm.
-}

import BasicEditorControls exposing (editorView)
import Browser
import DocumentNodeToEditorNode exposing (..)
import DocumentUtils
import EditorNodeToHtml exposing (..)
import HandleBeforeInput exposing (beforeInputDecoder, handleBeforeInput)
import HandleCompositionEnd exposing (handleCompositionEnd)
import HandleCompositionStart exposing (handleCompositionStart)
import HandleCut exposing (handleCut)
import HandleDocumentNodeChange exposing (decodeDocumentNodeChange, handleDocumentNodeChange)
import HandleKeyDown exposing (handleKeyDown)
import HandlePasteWithData exposing (handlePasteWithData)
import Html exposing (Html, div, node)
import Html.Attributes exposing (attribute, class, contenteditable)
import Html.Events exposing (on, onBlur, preventDefaultOn)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import List exposing (repeat)
import Model exposing (CharacterMetadata, CharacterStyle, Document, DocumentNode, Keypress, Msg(..), PasteWithData, Selection, emptyCharacterMetadata)
import Random
import Set exposing (Set)
import String exposing (length)
import Task
import Uuid exposing (Uuid)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscribe }


{-| Decoder for a selection object. Note that anchorNode and focusNode are DocumentNode ids, and the
anchorOffset and focusOffset have been modified from the original Selection API to account for
span offsets created when styling the text.
-}
selectionDecoder : D.Decoder Selection
selectionDecoder =
    D.map7 Selection
        (D.field "anchorOffset" D.int)
        (D.field "anchorNode" D.string)
        (D.field "focusOffset" D.int)
        (D.field "focusNode" D.string)
        (D.field "isCollapsed" D.bool)
        (D.field "rangeCount" D.int)
        (D.field "type" D.string)


port updateSelection : (E.Value -> msg) -> Sub msg



--- MODEL


type alias Model =
    Document


initialText1 : String
initialText1 =
    "This is editable plain text.  Here's a quote from Dune:"


initialText2 : String
initialText2 =
    "A beginning is a very delicate time. Know then, that it is the year 10191. The known universe is ruled by the Padishah Emperor Shaddam the Fourth, my father. In this time, the most precious substance in the universe is the spice Melange."


initialDocumentNodes : List DocumentNode
initialDocumentNodes =
    [ DocumentNode "documentNode0" (repeat (length initialText1) emptyCharacterMetadata) initialText1 "div", DocumentNode "documentNode1" (repeat (length initialText2) emptyCharacterMetadata) initialText2 "Blockquote" ]


initialDocument : Document
initialDocument =
    Document "" 1 0 initialDocumentNodes Nothing (CharacterMetadata Set.empty) False


init : () -> ( Model, Cmd Msg )
init s =
    ( initialDocument, Task.succeed Init |> Task.perform identity )



-- UPDATE


onInput : Html.Attribute Msg
onInput =
    Html.Events.onInput OnInput


compositionEndDecoder =
    D.at [ "data" ] (D.map OnCompositionEnd D.string)


onCompositionEnd =
    on "compositionend" compositionEndDecoder


onCompositionStart =
    on "compositionstart" (D.succeed OnCompositionStart)


onPasteWithData =
    on "pastewithdata"
        (D.map OnPasteWithData
            (D.map2 PasteWithData
                (D.at [ "detail", "text" ] D.string)
                (D.at [ "detail", "html" ] D.string)
            )
        )


updateOnRandom : Uuid -> Model -> ( Model, Cmd Msg )
updateOnRandom uuid model =
    ( { model | id = Uuid.toString uuid }, Cmd.none )


updateOnSelection : E.Value -> Model -> ( Model, Cmd Msg )
updateOnSelection selectionValue model =
    if model.isComposing then
        ( model, Cmd.none )

    else
        let
            selection =
                D.decodeValue selectionDecoder selectionValue
        in
        case selection of
            Err err ->
                ( model, Cmd.none )

            Ok s ->
                ( { model | selection = Just s }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            ( model, Random.generate OnRandom Uuid.uuidGenerator )

        OnRandom uuid ->
            updateOnRandom uuid model

        OnCopy ->
            -- Let the default behavior occur because there doesn't seem a way to be able to manipulate
            -- the clipboard data with Elm
            ( model, Cmd.none )

        OnCut ->
            -- Let the default cut behavior occur, but also delete the selection in the model
            -- and force a complete rerender since the state could have changed dramatically.
            ( handleCut model, Cmd.none )

        OnPaste ->
            -- Let the default behavior occur because there doesn't seem a way to be able to manipulate
            -- the clipboard data with Elm.  Instead we'll use the custom event pastewithdata
            -- to determine what we can paste.
            ( model, Cmd.none )

        OnPasteWithData v ->
            ( handlePasteWithData v model, Cmd.none )

        OnBeforeInput value ->
            handleBeforeInput value model

        OnCompositionStart ->
            handleCompositionStart model

        OnCompositionEnd s ->
            handleCompositionEnd s model

        OnInput v ->
            ( model, Cmd.none )

        OnDocumentNodeChange v ->
            ( handleDocumentNodeChange v model, Cmd.none )

        OnBlur ->
            ( model, Cmd.none )

        SelectionEvent v ->
            updateOnSelection v model

        OnKeyDown v ->
            handleKeyDown v model

        OnButtonPress v ->
            updateOnButtonPress v model

        Noop ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateOnButtonPress : String -> Model -> ( Model, Cmd Msg )
updateOnButtonPress buttonValue model =
    if List.member buttonValue [ "Bold", "Italic", "Underline", "Monospace" ] then
        let
            newStyles =
                toggleStyle model.currentStyles buttonValue

            newDocument =
                case model.selection of
                    Nothing ->
                        model

                    Just selection ->
                        DocumentUtils.setStylesOnSelection newStyles selection model
        in
        ( { newDocument | currentStyles = newStyles }, Cmd.none )

    else if List.member buttonValue [ "H1", "H2", "H3", "H4", "H5", "H6", "Blockquote" ] then
        ( toggleSelectedBlocks buttonValue model, Cmd.none )

    else
        ( model, Cmd.none )


toggleSelectedBlocks : String -> Document -> Document
toggleSelectedBlocks value document =
    case document.selection of
        Nothing ->
            document

        Just selection ->
            let
                ( before, selected, after ) =
                    DocumentUtils.getSelectionBlocks selection document.nodes
            in
            { document
                | renderCount = document.renderCount + 1
                , nodes =
                    before
                        ++ List.map
                            (\node ->
                                { node
                                    | nodeType =
                                        if node.nodeType == value then
                                            "div"

                                        else
                                            value
                                }
                            )
                            selected
                        ++ after
            }


toggleStyle : CharacterMetadata -> CharacterStyle -> CharacterMetadata
toggleStyle characterMetadata characterStyle =
    { characterMetadata
        | styles =
            if Set.member characterStyle characterMetadata.styles then
                Set.remove characterStyle characterMetadata.styles

            else
                Set.insert characterStyle characterMetadata.styles
    }



-- VIEW


doOnBlur =
    onBlur OnBlur


view : Model -> Html Msg
view model =
    div [] [ div [ Html.Attributes.class "rte-example" ] [ editorView model, renderDocument model ] ]


selectionAttributesIfPresent : Document -> List (Html.Attribute Msg)
selectionAttributesIfPresent d =
    case d.selection of
        Nothing ->
            []

        Just selection ->
            [ attribute "selection"
                (String.join ","
                    [ "focus-offset=" ++ String.fromInt selection.focusOffset
                    , "anchor-offset=" ++ String.fromInt selection.anchorOffset
                    , "anchor-node=" ++ selection.anchorNode
                    , "focus-node=" ++ selection.focusNode
                    , "is-collapsed="
                        ++ (if selection.isCollapsed then
                                "true"

                            else
                                "false"
                           )
                    , "range-count=" ++ String.fromInt selection.rangeCount
                    , "selection-type=" ++ selection.selectionType
                    , "render-count=" ++ String.fromInt d.renderCount
                    ]
                )
            ]


renderDocumentNodeToHtml : DocumentNode -> Html Msg
renderDocumentNodeToHtml =
    editorNodeToHtml << documentNodeToEditorNode


onCopy =
    on "copy" (D.succeed OnCopy)


onPaste =
    on "paste" (D.succeed OnPaste)


onCut =
    on "cut" (D.succeed OnCut)


onDocumentNodeChange =
    on "documentnodechange" decodeDocumentNodeChange


onBeforeInput =
    on "beforeinput" beforeInputDecoder



-- TODO: Implement drag/drop.  For now, we'll disable anything related to drag/drop


onDrag =
    preventDefaultOn "drag" (D.succeed ( Noop, True ))


onDrop =
    preventDefaultOn "drop" (D.succeed ( Noop, True ))


renderDocument : Document -> Html Msg
renderDocument document =
    div
        [ class "rte-container" ]
        [ Html.Keyed.node "div"
            [ contenteditable True
            , doOnBlur
            , onBeforeInput
            , HandleKeyDown.onKeyDown
            , onInput
            , class "rte-main"
            , onCompositionEnd
            , onCopy
            , onDrag
            , onDrop
            , onDocumentNodeChange
            , onPaste
            , onCut
            , onPasteWithData
            , onCompositionStart
            , attribute "data-rte" "true"
            , attribute "autocorrect" "off"
            , attribute "spellcheck" "false"
            , attribute "data-document-id" document.id
            ]
            [ ( String.fromInt document.renderCount, div [] (List.map renderDocumentNodeToHtml document.nodes) )
            ]
        , node "selection-state" (selectionAttributesIfPresent document) []
        ]



-- SUBSCRIPTIONS


subscribe : Model -> Sub Msg
subscribe model =
    Sub.batch [ updateSelection SelectionEvent ]

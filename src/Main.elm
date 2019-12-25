-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


port module Main exposing (updateSelection)

import BasicEditor exposing (editorView)
import Browser
import Debug as Debug
import DocumentNodeToEditorNode exposing (..)
import DocumentUtils
import EditorNodeToHtml exposing (..)
import HandleBeforeInput exposing (handleBeforeInput, onBeforeInput)
import HandleCompositionEnd exposing (handleCompositionEnd)
import HandleCompositionStart exposing (handleCompositionStart)
import HandleCut exposing (handleCut)
import HandleDocumentNodeChange exposing (decodeDocumentNodeChange, handleDocumentNodeChange)
import HandleKeyDown exposing (handleKeyDown)
import HandlePasteWithData exposing (handlePasteWithData)
import Html exposing (Html, div, node)
import Html.Attributes exposing (attribute, class, contenteditable, style)
import Html.Events exposing (on, onBlur, preventDefaultOn)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import List exposing (repeat)
import Model exposing (CharacterMetadata, CharacterStyle, Document, DocumentNode, DocumentNodeType, Keypress, Msg(..), PasteWithData, Selection, emptyCharacterMetadata)
import Random
import Set exposing (Set)
import String exposing (length)
import Task
import Uuid exposing (Uuid)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscribe }


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



-- MODEL


type alias Model =
    Document


initialText : String
initialText =
    "Write something here..."


initialDocumentNodes : List DocumentNode
initialDocumentNodes =
    [ DocumentNode "documentNode0" (repeat (length initialText) emptyCharacterMetadata) initialText "" ]


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



-- TODO parse selection in Elm instead of JS


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

        OnCopy v ->
            -- Let the default behavior occur because there doesn't seem a way to be able to manipulate
            -- the clipboard data
            ( model, Cmd.none )

        OnCut ->
            -- Let the default behavior occur, but delete the selection and force a rerender
            ( handleCut model, Cmd.none )

        OnPaste ->
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
                | nodes =
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


selectionAttributesIfPresent : Maybe Selection -> List (Html.Attribute Msg)
selectionAttributesIfPresent s =
    case s of
        Nothing ->
            []

        Just selection ->
            [ attribute "focus-offset" (String.fromInt selection.focusOffset)
            , attribute "anchor-offset" (String.fromInt selection.anchorOffset)
            , attribute "anchor-node" selection.anchorNode
            , attribute "focus-node" selection.focusNode
            , attribute "is-collapsed"
                (if selection.isCollapsed then
                    "true"

                 else
                    "false"
                )
            , attribute "range-count" (String.fromInt selection.rangeCount)
            , attribute "selection-type" selection.selectionType
            ]


renderDocumentNodeToHtml : DocumentNode -> Html Msg
renderDocumentNodeToHtml =
    editorNodeToHtml << documentNodeToEditorNode


neverPreventDefault : msg -> ( msg, Bool )
neverPreventDefault msg =
    ( msg, False )


onCopy =
    preventDefaultOn "copy" (D.map neverPreventDefault (D.map OnCopy D.value))


onPaste =
    on "paste" (D.succeed OnPaste)


onCut =
    on "cut" (D.succeed OnCut)


onDocumentNodeChange =
    on "documentnodechange" decodeDocumentNodeChange


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
            [ -- hangulbuffer exists because in Firefox on MacOS, Korean IME sometimes puts characters in the beginning of the content editable
              ( String.fromInt document.renderCount, div [] (List.map renderDocumentNodeToHtml document.nodes) )
            ]
        , node "selection-state" (selectionAttributesIfPresent document.selection) []
        ]



-- SUBSCRIPTIONS


subscribe : Model -> Sub Msg
subscribe model =
    Sub.batch [ updateSelection SelectionEvent ]

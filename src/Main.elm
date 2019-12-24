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
import EditorNodeToHtml exposing (..)
import HandleBeforeInput exposing (handleBeforeInput, onBeforeInput)
import HandleCompositionEnd exposing (handleCompositionEnd)
import HandleCompositionStart exposing (handleCompositionStart)
import HandleCut exposing (handleCut)
import HandleKeyDown exposing (handleKeyDown)
import HandlePasteWithData exposing (handlePasteWithData)
import Html exposing (Html, div, node)
import Html.Attributes exposing (attribute, contenteditable, id, style)
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
    "test text"


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
                Debug.log "Error parsing selection!" ( model, Cmd.none )

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
            Debug.log "on copy" ( model, Cmd.none )

        OnCut ->
            -- Let the default behavior occur, but delete the selection and force a rerender
            Debug.log "on cut" ( handleCut model, Cmd.none )

        OnPaste ->
            Debug.log "on paste" ( model, Cmd.none )

        OnPasteWithData v ->
            Debug.log "on pate with data" ( handlePasteWithData v model, Cmd.none )

        OnBeforeInput value ->
            let
                x =
                    Debug.log "beforeinputvalue" value
            in
            handleBeforeInput value model

        OnCompositionStart ->
            Debug.log "composition start" (handleCompositionStart model)

        OnCompositionEnd s ->
            Debug.log "composition end" (handleCompositionEnd s model)

        OnInput v ->
            Debug.log "on input" ( model, Cmd.none )

        OnBlur ->
            ( model, Cmd.none )

        SelectionEvent v ->
            updateOnSelection v model

        OnKeyDown v ->
            Debug.log "keydown" (handleKeyDown v model)

        OnButtonPress v ->
            updateOnButtonPress v model

        Noop ->
            Debug.log "noop" ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateOnButtonPress : String -> Model -> ( Model, Cmd Msg )
updateOnButtonPress buttonValue model =
    case buttonValue of
        "Bold" ->
            ( { model | currentStyles = toggleStyle model.currentStyles buttonValue }, Cmd.none )

        _ ->
            ( model, Cmd.none )


toggleStyle : CharacterMetadata -> CharacterStyle -> CharacterMetadata
toggleStyle characterMetadata characterStyle =
    { characterMetadata
        | styles =
            if Set.member characterStyle characterMetadata.styles then
                Set.remove characterStyle characterMetadata.styles

            else
                Set.insert characterStyle characterMetadata.styles
    }



-- Update the current style and current selection


updateStyle : CharacterStyle -> Model -> ( Model, Cmd Msg )
updateStyle characterStyle model =
    let
        newCurrentStyles =
            toggleStyle model.currentStyles characterStyle

        newModel =
            { model | currentStyles = newCurrentStyles }
    in
    ( newModel, Cmd.none )



-- VIEW


doOnBlur =
    onBlur OnBlur


view : Model -> Html Msg
view model =
    div [] [ editorView model, renderDocument model ]


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


onTestEvent =
    on "testevent" (D.succeed Noop)


neverPreventDefault : msg -> ( msg, Bool )
neverPreventDefault msg =
    ( msg, False )


onCopy =
    preventDefaultOn "copy" (D.map neverPreventDefault (D.map OnCopy D.value))


onPaste =
    on "paste" (D.succeed OnPaste)


onCut =
    on "cut" (D.succeed OnCut)


renderDocument : Document -> Html Msg
renderDocument document =
    div
        []
        [ Html.Keyed.node "div"
            [ contenteditable True
            , doOnBlur
            , onBeforeInput
            , onTestEvent
            , HandleKeyDown.onKeyDown
            , onInput
            , style "display" "inline-block"
            , onCompositionEnd
            , onCopy
            , onPaste
            , onCut
            , onPasteWithData
            , onCompositionStart
            , attribute "data-rte" "true"
            , attribute "spellcheck" "false"
            , attribute "data-document-id" document.id
            ]
            [ ( String.fromInt document.renderCount, div [] (List.map renderDocumentNodeToHtml document.nodes) ) ]
        , node "selection-state" (selectionAttributesIfPresent document.selection) []
        ]



-- SUBSCRIPTIONS


subscribe : Model -> Sub Msg
subscribe model =
    Sub.batch [ updateSelection SelectionEvent ]

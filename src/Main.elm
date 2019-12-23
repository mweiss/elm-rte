-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


port module Main exposing (tryIncomingPort, tryKeyDown, tryOutgoingPort)

import BasicEditor exposing (editorView)
import Browser
import Debug as Debug
import DocumentNodeToEditorNode exposing (..)
import EditorNodeToHtml exposing (..)
import HandleBeforeInput exposing (handleBeforeInput, onBeforeInput)
import HandleCompositionEnd exposing (handleCompositionEnd)
import HandleCompositionStart exposing (handleCompositionStart)
import HandleKeyDown exposing (handleKeyDown)
import Html exposing (Html, div, node)
import Html.Attributes exposing (attribute, contenteditable, id, style)
import Html.Events exposing (on, onBlur, preventDefaultOn)
import Json.Decode as D
import Json.Encode as E
import List exposing (drop, repeat, take)
import Model exposing (CharacterMetadata, CharacterStyle, Document, DocumentNode, DocumentNodeType, Keypress, Msg(..), Selection, emptyCharacterMetadata)
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


port tryOutgoingPort : String -> Cmd m


port tryIncomingPort : (E.Value -> msg) -> Sub msg


port tryKeyDown : (E.Value -> msg) -> Sub msg



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
    Document "" 1 initialDocumentNodes Nothing (CharacterMetadata Set.empty) False


init : () -> ( Model, Cmd Msg )
init s =
    ( initialDocument, Task.succeed Init |> Task.perform identity )



-- UPDATE


preventDefaultOnKeydown : Msg -> ( Msg, Bool )
preventDefaultOnKeydown msg =
    case msg of
        OnKeyDown keypress ->
            case keypress.key of
                "Backspace" ->
                    ( msg, True )

                "Delete" ->
                    ( msg, True )

                "Tab" ->
                    ( msg, True )

                "Enter" ->
                    ( msg, True )

                "Return" ->
                    ( msg, True )

                _ ->
                    ( msg, False )

        _ ->
            ( msg, False )


onKeyDownDecoder : D.Decoder Msg
onKeyDownDecoder =
    D.map (\x -> OnKeyDown x) HandleKeyDown.keyDownDecoder


onKeyDown : Html.Attribute Msg
onKeyDown =
    preventDefaultOn "keydown" (D.map preventDefaultOnKeydown onKeyDownDecoder)


compositionEndDecoder =
    D.at [ "data" ] (D.map OnCompositionEnd D.string)


onCompositionEnd =
    on "compositionend" compositionEndDecoder


onCompositionStart =
    on "compositionstart" (D.succeed OnCompositionStart)


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

        OnBeforeInput value ->
            Debug.log "on before input" (handleBeforeInput value model)

        OnCompositionStart ->
            Debug.log "composition start" (handleCompositionStart model)

        OnCompositionEnd s ->
            Debug.log "composition end" (handleCompositionEnd s model)

        OnBlur ->
            ( model, tryOutgoingPort "blur" )

        SelectionEvent v ->
            updateOnSelection v model

        OnKeyDown v ->
            Debug.log "keydown" (handleKeyDown v model)

        OnButtonPress v ->
            updateOnButtonPress v model

        Noop ->
            Debug.log "noop" ( model, Cmd.none )

        _ ->
            ( model, tryOutgoingPort "test" )


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


renderDocument : Document -> Html Msg
renderDocument document =
    div
        []
        [ div
            [ contenteditable True
            , doOnBlur
            , onBeforeInput
            , onTestEvent
            , onKeyDown
            , style "display" "inline-block"
            , onCompositionEnd
            , onCompositionStart
            , attribute "data-rte" "true"
            , attribute "data-document-id" document.id
            ]
            (List.map renderDocumentNodeToHtml document.nodes)
        , node "selection-state" (selectionAttributesIfPresent document.selection) []
        ]



-- SUBSCRIPTIONS


subscribe : Model -> Sub Msg
subscribe model =
    Sub.batch [ tryIncomingPort SelectionEvent, tryKeyDown KeyDownEvent ]

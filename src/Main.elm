-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


port module Main exposing (tryIncomingPort, tryKeyPress, tryOutgoingPort)

import BasicEditor exposing (editorView)
import Browser
import Debug as Debug
import DocumentNodeToEditorNode exposing (..)
import EditorNodeToHtml exposing (..)
import Html exposing (Html, div, node)
import Html.Attributes exposing (attribute, contenteditable, id, style)
import Html.Events exposing (on, onBlur)
import Json.Decode as D
import Json.Encode as E
import List exposing (drop, repeat, take)
import Model exposing (CharacterMetadata, CharacterStyle, Document, DocumentNode, DocumentNodeType, Keypress, Msg(..), Selection, emptyCharacterMetadata)
import Random
import Set exposing (Set)
import String exposing (length)
import String.Extra exposing (insertAt)
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


keyPressDecoder : D.Decoder Keypress
keyPressDecoder =
    D.map2 Keypress
        (D.field "keyCode" D.int)
        (D.field "key" D.string)


port tryOutgoingPort : String -> Cmd m


port tryIncomingPort : (E.Value -> msg) -> Sub msg


port tryKeyPress : (E.Value -> msg) -> Sub msg



-- MODEL


type alias Model =
    Document


initialText : String
initialText =
    "test text"


initialDocumentNodes : List DocumentNode
initialDocumentNodes =
    [ DocumentNode "documentNode0" (repeat (length initialText) emptyCharacterMetadata) initialText "" ]



-- Random.generate OnRandom Uuid.uuidGenerator


initialDocument : Document
initialDocument =
    Document "" 1 initialDocumentNodes Nothing (CharacterMetadata Set.empty)


init : () -> ( Model, Cmd Msg )
init s =
    ( initialDocument, Task.succeed Init |> Task.perform identity )



-- UPDATE


beforeInputDecoder =
    D.at [ "data" ] (D.map OnBeforeInput D.string)


onBeforeInput =
    on "beforeinput" beforeInputDecoder


keyDownDecoder =
    D.at [ "keyCode" ] (D.map OnKeyDown D.string)


onKeyDown =
    on "keydown" keyDownDecoder


compositionEndDecoder =
    D.at [ "data" ] (D.map OnCompositionEnd D.string)


onCompositionEnd =
    on "compositionend" compositionEndDecoder


updateOnRandom : Uuid -> Model -> ( Model, Cmd Msg )
updateOnRandom uuid model =
    ( { model | id = Uuid.toString uuid }, Cmd.none )



-- TODO parse selection in Elm instead of JS


updateOnSelection : E.Value -> Model -> ( Model, Cmd Msg )
updateOnSelection selectionValue model =
    let
        selection =
            Debug.log "Test selection" (D.decodeValue selectionDecoder selectionValue)
    in
    case selection of
        Err err ->
            Debug.log "Error parsing selection!" ( model, Cmd.none )

        Ok s ->
            ( { model | selection = Just s }, Cmd.none )


mapDocument : (DocumentNode -> DocumentNode) -> Document -> Document
mapDocument fn document =
    { document | nodes = List.map fn document.nodes }


insertIfSelected : Selection -> CharacterMetadata -> String -> DocumentNode -> DocumentNode
insertIfSelected selection cm s node =
    if String.startsWith node.id selection.focusNode then
        { node
            | text = insertAt s selection.focusOffset node.text
            , characterMetadata = take selection.focusOffset node.characterMetadata ++ repeat (length s) cm ++ drop selection.focusOffset node.characterMetadata
        }

    else
        node


updateOnKeyPress : E.Value -> Model -> ( Model, Cmd Msg )
updateOnKeyPress keypressValue model =
    let
        keyPress =
            Debug.log "Testing incoming port" (D.decodeValue keyPressDecoder keypressValue)
    in
    case keyPress of
        Err err ->
            Debug.log "Error parsing key press" ( model, Cmd.none )

        Ok keypress ->
            -- TODO: handle all cases
            case model.selection of
                Nothing ->
                    ( model, Cmd.none )

                Just selection ->
                    let
                        td =
                            mapDocument (insertIfSelected selection model.currentStyles keypress.key) model

                        newSel =
                            { selection | focusOffset = selection.focusOffset + 1, anchorOffset = selection.anchorOffset + 1 }

                        -- TODO make length correct instead of +1
                        newDoc =
                            { td | selection = Just newSel }
                    in
                    ( newDoc, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            ( model, Random.generate OnRandom Uuid.uuidGenerator )

        OnRandom uuid ->
            updateOnRandom uuid model

        OnBeforeInput s ->
            ( Debug.log "Testing debug message before input" model, tryOutgoingPort "bi" )

        OnKeyDown s ->
            ( Debug.log "Testing debug message keydown" model, tryOutgoingPort "kd" )

        OnCompositionEnd s ->
            ( Debug.log "Testing debug message composition end" model, tryOutgoingPort "ke" )

        OnBlur ->
            ( model, tryOutgoingPort "blur" )

        SelectionEvent v ->
            updateOnSelection v model

        KeyPressEvent v ->
            updateOnKeyPress v model

        OnButtonPress v ->
            updateOnButtonPress v model

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


renderDocument : Document -> Html Msg
renderDocument document =
    div
        []
        [ div
            [ contenteditable True
            , doOnBlur
            , onBeforeInput
            , onKeyDown
            , style "display" "inline-block"
            , onCompositionEnd
            , attribute "data-rte" "true"
            , attribute "data-document-id" document.id
            ]
            (List.map renderDocumentNodeToHtml document.nodes)
        , node "selection-state" (selectionAttributesIfPresent document.selection) []
        ]



-- SUBSCRIPTIONS


subscribe : Model -> Sub Msg
subscribe model =
    Sub.batch [ tryIncomingPort SelectionEvent, tryKeyPress KeyPressEvent ]

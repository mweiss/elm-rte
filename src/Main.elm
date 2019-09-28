-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


port module Main exposing (tryIncomingPort, tryKeyPress, tryOutgoingPort)

import Browser
import Browser.Dom
import Browser.Events
import Debug as Debug
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, contenteditable)
import Html.Events exposing (on, onBlur, onClick)
import Html.Keyed exposing (node)
import Json.Decode as D
import Json.Encode as E
import Random
import Set exposing (Set)
import Task
import Uuid exposing (Uuid, uuidGenerator)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscribe }


type alias Selection =
    { anchorOffset : Int
    , anchorNode : String
    , focusOffset : Int
    , focusNode : String
    , isCollapsed : Bool
    , rangeCount : Int
    , selectionType : String
    }


type alias Keypress =
    { keyCode : Int
    , key : String
    }


type alias Document =
    { id : String
    , idCounter : Int
    , nodes : List DocumentNode
    }


type DocumentNodeType
    = Paragraph
    | Header


type alias DocumentNode =
    { id : String
    , characterMetadata : CharacterMetadata
    , text : String
    , nodeType : String
    }


type CharacterStyle
    = Bold
    | Italic


type alias CharacterMetadata =
    { styles : Set CharacterStyle
    }


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


type UuidTest
    = UuidTestV Int


initialDocumentNodes : List DocumentNode
initialDocumentNodes =
    [ DocumentNode "0" (CharacterMetadata Set.empty) "test text" "" ]



-- Random.generate OnRandom Uuid.uuidGenerator


initialDocument : Document
initialDocument =
    Document "" 1 initialDocumentNodes


init : () -> ( Model, Cmd Msg )
init s =
    ( initialDocument, Task.succeed Init |> Task.perform identity )



-- UPDATE


type Msg
    = OnBeforeInput String
    | Init
    | OnRandom Uuid
    | OnBlur
    | OnCompositionStart
    | OnCopy
    | OnCut
    | OnDragOver
    | OnDragStart
    | OnFocus
    | OnInput
    | OnPaste
    | OnSelect
    | OnKeyDown String
    | OnCompositionEnd String
    | Noop
    | SelectionEvent E.Value
    | KeyPressEvent E.Value


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
            let
                t =
                    Debug.log "Testing incoming port" (D.decodeValue selectionDecoder v)
            in
            ( model, tryOutgoingPort "test" )

        KeyPressEvent v ->
            let
                t =
                    Debug.log "Testing incoming port" (D.decodeValue keyPressDecoder v)
            in
            ( model, tryOutgoingPort "test" )

        _ ->
            ( model, tryOutgoingPort "test" )



-- VIEW


doOnBlur =
    onBlur OnBlur


view : Model -> Html Msg
view model =
    renderDocument model


renderDocument : Document -> Html Msg
renderDocument document =
    node "div"
        []
        [ ( document.id
          , node "div"
                [ contenteditable True
                , doOnBlur
                , onBeforeInput
                , onKeyDown
                , onCompositionEnd
                , attribute "data-rte" "true"
                , attribute "data-document-id" document.id
                ]
                (List.map renderDocumentNode document.nodes)
          )
        ]


renderDocumentNode : DocumentNode -> ( String, Html Msg )
renderDocumentNode documentNode =
    ( documentNode.id
    , div
        [ attribute "data-document-node-id" documentNode.id ]
        [ text documentNode.text ]
    )



-- SUBSCRIPTIONS


subscribe : Model -> Sub Msg
subscribe model =
    Sub.batch [ tryIncomingPort SelectionEvent, tryKeyPress KeyPressEvent ]

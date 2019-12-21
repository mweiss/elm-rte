module HandleKeyDown exposing (handleKeyDown)

import Debug as Debug
import DocumentUtils exposing (backspace, backspaceWord, delete, insertIfSelected, mapDocument, splitBlock)
import Json.Decode as D
import Json.Encode as E
import List exposing (drop, repeat, take)
import Model exposing (..)
import String exposing (length)
import String.Extra exposing (insertAt)


keyDownDecoder : D.Decoder Keypress
keyDownDecoder =
    D.map5 Keypress
        (D.field "keyCode" D.int)
        (D.field "key" D.string)
        (D.field "altKey" D.bool)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)



-- Handle key codes
-- RETURN, DELETE, TAB, BACKSPACE, UP, DOWN, LEFT, RIGHT, SHIFT, CAPSLOCK, ALT, OPTION, COMMAND, CTRL,
-- TODO: implement me!


handleEnter : Document -> Selection -> ( Document, Cmd Msg )
handleEnter model selection =
    let
        newModel =
            splitBlock selection model
    in
    ( newModel, Cmd.none )


handleTab : Document -> Selection -> ( Document, Cmd Msg )
handleTab model selection =
    ( model, Cmd.none )


handleBackspace : Document -> Selection -> ( Document, Cmd Msg )
handleBackspace model selection =
    ( backspace selection model, Cmd.none )


handleBackspaceWord : Document -> Selection -> ( Document, Cmd Msg )
handleBackspaceWord model selection =
    ( backspaceWord selection model, Cmd.none )


handleDelete : Document -> Selection -> ( Document, Cmd Msg )
handleDelete model selection =
    ( delete selection model, Cmd.none )


handleKeyDown : E.Value -> Document -> ( Document, Cmd Msg )
handleKeyDown keypressValue model =
    let
        keyPress =
            Debug.log "Testing incoming port" (D.decodeValue keyDownDecoder keypressValue)
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
                    case keypress.key of
                        "Backspace" ->
                            if keypress.ctrlKey then
                                handleBackspaceWord model selection

                            else
                                handleBackspace model selection

                        "Delete" ->
                            handleDelete model selection

                        "Tab" ->
                            handleTab model selection

                        "Enter" ->
                            handleEnter model selection

                        "Return" ->
                            handleEnter model selection

                        _ ->
                            ( model, Cmd.none )

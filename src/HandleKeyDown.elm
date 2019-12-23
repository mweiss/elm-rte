module HandleKeyDown exposing (handleKeyDown, keyDownDecoder)

import Debug as Debug
import DocumentUtils exposing (backspace, backspaceWord, delete, deleteWord, splitBlock)
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


handleDeleteWord : Document -> Selection -> ( Document, Cmd Msg )
handleDeleteWord model selection =
    ( deleteWord selection model, Cmd.none )


handleKeyDown : Keypress -> Document -> ( Document, Cmd Msg )
handleKeyDown keypress model =
    case model.selection of
        Nothing ->
            ( model, Cmd.none )

        Just selection ->
            if model.isComposing then
                ( model, Cmd.none )

            else
                case keypress.key of
                    "Backspace" ->
                        if keypress.ctrlKey then
                            handleBackspaceWord model selection

                        else
                            handleBackspace model selection

                    "Delete" ->
                        if keypress.ctrlKey then
                            handleDeleteWord model selection

                        else
                            handleDelete model selection

                    "Tab" ->
                        handleTab model selection

                    "Enter" ->
                        handleEnter model selection

                    "Return" ->
                        handleEnter model selection

                    _ ->
                        ( model, Cmd.none )

module HandleKeyDown exposing (handleKeyDown, keyDownDecoder, onKeyDown)

{-
   This module handles user actions like backspace, delete, enter, as well in the future commands
   like undo, redo, or any hot keys.  We handle these type of actions through the keydown event,
   while input is handled mostly through the beforeinput event.
-}

import DocumentUtils exposing (backspace, backspaceWord, delete, deleteWord, splitBlock)
import Html
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Model exposing (..)
import Set


onKeyDownDecoder : D.Decoder Msg
onKeyDownDecoder =
    D.map (\x -> OnKeyDown x) keyDownDecoder


preventDefaultOnKeydown : Msg -> ( Msg, Bool )
preventDefaultOnKeydown msg =
    case msg of
        OnKeyDown keypress ->
            if keypress.isComposing then
                ( msg, False )

            else
                case keypress.key of
                    "b" ->
                        ( msg, keypress.metaKey )

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


onKeyDown : Html.Attribute Msg
onKeyDown =
    preventDefaultOn "keydown" (D.map preventDefaultOnKeydown onKeyDownDecoder)


keyDownDecoder : D.Decoder Keypress
keyDownDecoder =
    D.map6 Keypress
        (D.field "keyCode" D.int)
        (D.field "key" D.string)
        (D.field "altKey" D.bool)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.oneOf [ D.field "isComposing" D.bool, D.succeed False ])


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


handleBold : Document -> Selection -> ( Document, Cmd Msg )
handleBold model selection =
    ( DocumentUtils.setStylesOnSelection { styles = Set.fromList [ "Bold" ] } selection model, Cmd.none )


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
                    "b" ->
                        if keypress.metaKey then
                            handleBold model selection

                        else
                            ( model, Cmd.none )

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

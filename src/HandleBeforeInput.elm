module HandleBeforeInput exposing (..)

import DocumentUtils exposing (backspace, backspaceWord, delete, deleteWord, insertAtSelection, splitBlock)
import Json.Decode as D
import Model exposing (Document, InputEvent, Msg(..))


beforeInputDecoder : D.Decoder Msg
beforeInputDecoder =
    D.map OnBeforeInput
        (D.map3 InputEvent
            (D.field "data" D.string)
            (D.oneOf [ D.field "isComposing" D.bool, D.succeed False ])
            (D.field "inputType" D.string)
        )


handleBeforeInput : InputEvent -> Document -> ( Document, Cmd Msg )
handleBeforeInput beforeInput model =
    if model.isComposing then
        ( model, Cmd.none )

    else if beforeInput.inputType == "insertText" || beforeInput.inputType == "insertCompositionText" || beforeInput.inputType == "insertReplacementText" then
        ( insertAtSelection beforeInput.data model, Cmd.none )

    else if beforeInput.inputType == "insertParagraph" || beforeInput.inputType == "insertLineBreak" then
        case model.selection of
            Nothing ->
                ( model, Cmd.none )

            Just selection ->
                ( splitBlock selection model, Cmd.none )

    else if beforeInput.inputType == "deleteContent" || beforeInput.inputType == "deleteContentBackward" then
        case model.selection of
            Nothing ->
                ( model, Cmd.none )

            Just selection ->
                ( backspace selection model, Cmd.none )

    else if beforeInput.inputType == "deleteContentForward" then
        case model.selection of
            Nothing ->
                ( model, Cmd.none )

            Just selection ->
                ( delete selection model, Cmd.none )

    else if beforeInput.inputType == "deleteWordBackward" then
        case model.selection of
            Nothing ->
                ( model, Cmd.none )

            Just selection ->
                ( backspaceWord selection model, Cmd.none )

    else if beforeInput.inputType == "deleteWordForward" then
        case model.selection of
            Nothing ->
                ( model, Cmd.none )

            Just selection ->
                ( deleteWord selection model, Cmd.none )

    else
        ( model, Cmd.none )

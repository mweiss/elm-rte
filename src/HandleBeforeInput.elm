module HandleBeforeInput exposing (..)

import DocumentUtils exposing (insertAtSelection)
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

    else
        ( insertAtSelection beforeInput.data model, Cmd.none )

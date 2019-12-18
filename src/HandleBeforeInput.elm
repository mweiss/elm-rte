module HandleBeforeInput exposing (..)

import DocumentUtils exposing (insertIfSelected, mapDocument)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Model exposing (BeforeInput, Document, Msg(..))


beforeInputDecoder : D.Decoder ( Msg, Bool )
beforeInputDecoder =
    D.map alwaysPreventDefault
        (D.map OnBeforeInput
            (D.map3 BeforeInput
                (D.field "data" D.string)
                (D.field "isComposing" D.bool)
                (D.field "inputType" D.string)
            )
        )


onBeforeInput =
    preventDefaultOn "beforeinput" beforeInputDecoder



-- For now, always prevent the default from happening


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


handleBeforeInput : BeforeInput -> Document -> ( Document, Cmd Msg )
handleBeforeInput beforeInput model =
    case model.selection of
        Nothing ->
            ( model, Cmd.none )

        Just selection ->
            let
                td =
                    mapDocument (insertIfSelected selection model.currentStyles beforeInput.data) model

                newSel =
                    { selection | focusOffset = selection.focusOffset + 1, anchorOffset = selection.anchorOffset + 1 }

                newDoc =
                    { td | selection = Just newSel }
            in
            ( newDoc, Cmd.none )

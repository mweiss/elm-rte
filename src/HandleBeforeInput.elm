module HandleBeforeInput exposing (..)

import DocumentUtils exposing (insertAtSelection)
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



-- Handle insertion cases
-- on composition
-- when range is selected


handleBeforeInput : BeforeInput -> Document -> ( Document, Cmd Msg )
handleBeforeInput beforeInput model =
    if beforeInput.isComposing then
        ( model, Cmd.none )

    else
        ( insertAtSelection beforeInput.data model, Cmd.none )

module HandleBeforeInput exposing (..)

import DocumentUtils exposing (insertIfSelected, mapDocument)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Model exposing (Document, Msg(..))


beforeInputDecoder =
    D.map alwaysPreventDefault (D.at [ "data" ] (D.map OnBeforeInput D.string))


onBeforeInput =
    preventDefaultOn "beforeinput" beforeInputDecoder



-- For now, always prevent the default from happening


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


handleBeforeInput : String -> Document -> ( Document, Cmd Msg )
handleBeforeInput data model =
    -- TODO: handle all cases
    case model.selection of
        Nothing ->
            ( model, Cmd.none )

        Just selection ->
            let
                td =
                    mapDocument (insertIfSelected selection model.currentStyles data) model

                newSel =
                    { selection | focusOffset = selection.focusOffset + 1, anchorOffset = selection.anchorOffset + 1 }

                newDoc =
                    { td | selection = Just newSel }
            in
            ( newDoc, Cmd.none )

module BasicEditor exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Model exposing (Document, Msg(..))


editorView : Document -> Html Msg
editorView model =
    div [] [ editorPanel ]


editorPanel =
    div []
        [ button [ onClick (OnButtonPress "H1") ] [ text "H1" ]
        , button [ onClick (OnButtonPress "H2") ] [ text "H2" ]
        , button [ onClick (OnButtonPress "H3") ] [ text "H3" ]
        , button [ onClick (OnButtonPress "H4") ] [ text "H4" ]
        , button [ onClick (OnButtonPress "H5") ] [ text "H5" ]
        , button [ onClick (OnButtonPress "H6") ] [ text "H6" ]
        , button [ onClick (OnButtonPress "Blockquote") ] [ text "Blockquote" ]
        , button [ onClick (OnButtonPress "UL") ] [ text "UL" ]
        , button [ onClick (OnButtonPress "OL") ] [ text "OL" ]
        , button [ onClick (OnButtonPress "Code Block") ] [ text "Code Block" ]
        , button [ onClick (OnButtonPress "Bold") ] [ text "Bold" ]
        , button [ onClick (OnButtonPress "Italic") ] [ text "Italic" ]
        , button [ onClick (OnButtonPress "Underscore") ] [ text "Underscore" ]
        , button [ onClick (OnButtonPress "Monospace") ] [ text "Monospace" ]
        ]



-- TODO create buttons for H1 H2… etc, buttons, also create text fields

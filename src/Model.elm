module Model exposing (..)

import Set exposing (Set)


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
    , selection : Maybe Selection
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

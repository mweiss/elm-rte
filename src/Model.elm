module Model exposing (..)

import Dict exposing (Dict)
import Json.Encode as E
import Set exposing (Set)
import Uuid exposing (Uuid)


type EditorNode
    = BlockEditorNode
        { id : String
        , childNodes : List EditorNode
        , styles : Dict String String
        , nodeType : String
        }
    | LeafEditorNode
        { offset : Int
        , styles : Dict String String
        , text : String
        , id : String
        }


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
    , altKey : Bool
    , metaKey : Bool
    , ctrlKey : Bool
    }


type alias Document =
    { id : String
    , idCounter : Int
    , nodes : List DocumentNode
    , selection : Maybe Selection
    , currentStyles : CharacterMetadata
    }


type DocumentNodeType
    = Paragraph
    | Header


type alias DocumentNode =
    { id : String
    , characterMetadata : List CharacterMetadata
    , text : String
    , nodeType : String
    }


type alias CharacterStyle =
    String


type alias CharacterMetadata =
    { styles : Set CharacterStyle
    }


emptyCharacterMetadata : CharacterMetadata
emptyCharacterMetadata =
    { styles = Set.empty }


type Msg
    = OnBeforeInput String
    | Init
    | OnRandom Uuid
    | OnBlur
    | OnCompositionStart
    | OnCopy
    | OnCut
    | OnDragOver
    | OnDragStart
    | OnFocus
    | OnInput
    | OnPaste
    | OnSelect
    | OnKeyDown String
    | OnCompositionEnd String
    | Noop
    | SelectionEvent E.Value
    | KeyDownEvent E.Value
    | OnButtonPress String

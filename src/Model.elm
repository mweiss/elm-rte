module Model exposing (..)

{-
   This module contains the model used by the rich text editor.
-}

import Dict exposing (Dict)
import Json.Encode as E
import Set exposing (Set)
import Uuid exposing (Uuid)



{-
   EditorNode is an intermediary format used for rendering.  It is not used to represent the document
   itself, it's just a convenient way to store what we want to render before turning it to Html.
-}


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



{-
   Type alias for a selection object.
-}


type alias Selection =
    { anchorOffset : Int
    , anchorNode : String
    , focusOffset : Int
    , focusNode : String
    , isCollapsed : Bool
    , rangeCount : Int
    , selectionType : String
    }



{-
   Type alias for a keypress event.
-}


type alias Keypress =
    { keyCode : Int
    , key : String
    , altKey : Bool
    , metaKey : Bool
    , ctrlKey : Bool
    , isComposing : Bool
    }



{-
   Type alias for a document.  A document represents the state of the editor.
-}


type alias Document =
    { id : String
    , idCounter : Int
    , renderCount : Int
    , nodes : List DocumentNode
    , selection : Maybe Selection
    , currentStyles : CharacterMetadata
    , isComposing : Bool
    }



{-
   Type alias for a document node.  A document node represents a block in the editor, like a paragraph,
   blockquote, or div.
-}


type alias DocumentNode =
    { id : String
    , characterMetadata : List CharacterMetadata
    , text : String
    , nodeType : String
    }


type alias CharacterStyle =
    String



{-
   CharacterMetadata stores style information about characters in the document.  Note, this will
   most likely hold other information like entity information.  It's based off of the draftjs
   object of the same name.
-}


type alias CharacterMetadata =
    { styles : Set CharacterStyle
    }


emptyCharacterMetadata : CharacterMetadata
emptyCharacterMetadata =
    { styles = Set.empty }



{- An input event type -}


type alias InputEvent =
    { data : String, isComposing : Bool, inputType : String }



{- A pastewithdata event object -}


type alias PasteWithData =
    { text : String, html : String }



{- A documentnodechange event object -}


type alias DocumentNodeChange =
    { node : String, text : String }


type Msg
    = OnBeforeInput InputEvent
    | Init
    | OnRandom Uuid
    | OnBlur
    | OnPasteWithData PasteWithData
    | OnDocumentNodeChange DocumentNodeChange
    | OnCompositionStart
    | OnCopy
    | OnCut
    | OnDragOver -- not implemented
    | OnDragStart -- not implemented
    | OnFocus -- not implemented
    | OnInput String
    | OnPaste
    | OnSelect -- not implemented
    | OnKeyDown Keypress
    | OnCompositionEnd String
    | Noop
    | SelectionEvent E.Value
    | OnButtonPress String

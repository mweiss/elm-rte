module DocumentUtils exposing (..)

import List exposing (drop, repeat, take)
import Model exposing (..)
import String exposing (length)
import String.Extra exposing (insertAt)


mapDocument : (DocumentNode -> DocumentNode) -> Document -> Document
mapDocument fn document =
    { document | nodes = List.map fn document.nodes }


insertIfSelected : Selection -> CharacterMetadata -> String -> DocumentNode -> DocumentNode
insertIfSelected selection cm s node =
    if String.startsWith node.id selection.focusNode then
        { node
            | text = insertAt s selection.focusOffset node.text
            , characterMetadata = take selection.focusOffset node.characterMetadata ++ repeat (length s) cm ++ drop selection.focusOffset node.characterMetadata
        }

    else
        node

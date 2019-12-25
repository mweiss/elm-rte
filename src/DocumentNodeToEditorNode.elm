module DocumentNodeToEditorNode exposing (..)

{-
   This module converts DocumentNodes to an intermediary format, EditorNodes.  This helps conceptually
   render it to HTML since going directly from document nodes to html requires a lot more steps.
-}

import Array
import Dict exposing (Dict)
import Model exposing (..)
import Set exposing (..)



{-
   Currently we have no specific styles for document nodes.
-}


getNodeStyles : String -> Dict String String
getNodeStyles nodeType =
    Dict.empty



{-
   This method derives the html node type from the documentNode node type.
-}


getNodeType : String -> String
getNodeType nodeType =
    case nodeType of
        "H1" ->
            "h1"

        "H2" ->
            "h2"

        "H3" ->
            "h3"

        "H4" ->
            "h4"

        "H5" ->
            "h5"

        "H6" ->
            "h6"

        "Blockquote" ->
            "blockquote"

        _ ->
            "div"


documentNodeToEditorNode : DocumentNode -> EditorNode
documentNodeToEditorNode documentNode =
    BlockEditorNode
        { id = documentNode.id
        , childNodes = createChildNodes documentNode
        , styles = getNodeStyles documentNode.nodeType
        , nodeType = getNodeType documentNode.nodeType
        }


foldCharactersWithSameMetadata : ( Char, CharacterMetadata ) -> Array.Array ( String, CharacterMetadata ) -> Array.Array ( String, CharacterMetadata )
foldCharactersWithSameMetadata ( char, metadata ) agg =
    case Array.get (Array.length agg - 1) agg of
        Nothing ->
            Array.fromList [ ( String.fromChar char, metadata ) ]

        Just ( prevChar, prevMetadata ) ->
            if prevMetadata == metadata then
                Array.set (Array.length agg - 1) ( String.concat [ prevChar, String.fromChar char ], prevMetadata ) agg

            else
                Array.push ( String.fromChar char, metadata ) agg


characterMetadataFunc : String -> Dict String String -> Dict String String
characterMetadataFunc style dict =
    case style of
        "Bold" ->
            Dict.insert "font-weight" "bold" dict

        "Italic" ->
            Dict.insert "font-style" "italic" dict

        "Underline" ->
            Dict.insert "text-decoration" "underline" dict

        "Monospace" ->
            Dict.insert "font-family" "monospace" dict

        _ ->
            dict


characterMetadataToStyles : CharacterMetadata -> Dict String String
characterMetadataToStyles characterMetadata =
    Set.foldl characterMetadataFunc Dict.empty characterMetadata.styles


charactersToNodes : String -> ( String, CharacterMetadata ) -> Array.Array EditorNode -> Array.Array EditorNode
charactersToNodes id ( text, characterMetadata ) agg =
    let
        offset =
            case Array.get (Array.length agg - 1) agg of
                Just (LeafEditorNode node) ->
                    String.length node.text + node.offset

                _ ->
                    0
    in
    Array.push
        (LeafEditorNode { offset = offset, text = text, styles = characterMetadataToStyles characterMetadata, id = id })
        agg


createChildNodes : DocumentNode -> List EditorNode
createChildNodes documentNode =
    let
        characterList =
            String.toList documentNode.text

        charactersWithMetadata =
            List.map2 Tuple.pair characterList documentNode.characterMetadata

        joinedCharactersWithSameMetadata =
            Array.foldl foldCharactersWithSameMetadata Array.empty (Array.fromList charactersWithMetadata)
    in
    Array.toList (Array.foldl (charactersToNodes documentNode.id) Array.empty joinedCharactersWithSameMetadata)

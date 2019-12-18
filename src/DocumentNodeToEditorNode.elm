module DocumentNodeToEditorNode exposing (..)

import Array
import Dict exposing (Dict, insert)
import Model exposing (..)
import Set exposing (..)



-- for now, no custom styles. TODO: figure out what to do here.


getNodeStyles : String -> Dict String String
getNodeStyles nodeType =
    Dict.empty



-- TODO: figure out what to do here as well


getNodeType : String -> String
getNodeType nodeType =
    "div"


documentNodeToEditorNode : DocumentNode -> EditorNode
documentNodeToEditorNode documentNode =
    BlockEditorNode
        { id = documentNode.id
        , childNodes = createChildNodes documentNode
        , styles = getNodeStyles documentNode.nodeType
        , nodeType = getNodeType documentNode.nodeType
        }



-- TODO: implement me!
-- YIKES this is really inefficient


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



-- TODO: implement me!


characterMetadataFunc : String -> Dict String String -> Dict String String
characterMetadataFunc style dict =
    case style of
        "Bold" ->
            Dict.insert "font-weight" "bold" dict

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

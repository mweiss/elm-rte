module DocumentUtils exposing (..)

import Array exposing (Array)
import List exposing (drop, repeat, take)
import List.Extra exposing (selectSplit)
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


getSelectionBlocks : Selection -> List DocumentNode -> ( List DocumentNode, List DocumentNode, List DocumentNode )
getSelectionBlocks selection documentNodes =
    if selection.focusNode == selection.anchorNode then
        getSelectionSingleBlock selection.focusNode documentNodes

    else
        getSelectionRangeBlocks selection documentNodes


getSelectionSingleBlock : String -> List DocumentNode -> ( List DocumentNode, List DocumentNode, List DocumentNode )
getSelectionSingleBlock nodeId documentNodes =
    let
        ( before, after ) =
            List.Extra.splitAt
                (Maybe.withDefault 0 (List.Extra.findIndex (\node -> node.id == nodeId) documentNodes))
                documentNodes

        ( found, end ) =
            List.Extra.splitAt 1 after
    in
    ( before, found, end )


getSelectionRangeBlocks : Selection -> List DocumentNode -> ( List DocumentNode, List DocumentNode, List DocumentNode )
getSelectionRangeBlocks selection documentNodes =
    let
        splitBefore =
            List.Extra.splitWhen (\node -> node.id == selection.anchorNode || node.id == selection.focusNode) documentNodes
    in
    case splitBefore of
        Nothing ->
            ( documentNodes, [], [] )

        Just ( before, after ) ->
            let
                foundId =
                    case List.head after of
                        Nothing ->
                            ""

                        Just v ->
                            v.id

                splitAfter =
                    List.Extra.splitWhen
                        (\node ->
                            node.id
                                /= foundId
                                && (node.id == selection.anchorNode || node.id == selection.focusNode)
                        )
                        after
            in
            case splitAfter of
                Nothing ->
                    ( before, after, [] )

                Just ( selectionEnd, listEnd ) ->
                    case List.head listEnd of
                        Nothing ->
                            ( before, after, [] )

                        Just v ->
                            ( before, selectionEnd ++ [ v ], List.drop 1 listEnd )



-- This should only be called when the list is larger than 1


splitSelectedRange : Selection -> List DocumentNode -> List DocumentNode
splitSelectedRange selection documentNodes =
    let
        first =
            List.head documentNodes

        last =
            List.Extra.last documentNodes
    in
    case first of
        Nothing ->
            documentNodes

        Just firstNode ->
            case last of
                Nothing ->
                    documentNodes

                Just lastNode ->
                    let
                        firstOffset =
                            if selection.focusNode == firstNode.id then
                                selection.focusOffset

                            else
                                selection.anchorOffset

                        lastOffset =
                            if selection.focusNode == lastNode.id then
                                selection.focusOffset

                            else
                                selection.anchorOffset

                        newFirstText =
                            String.left firstOffset firstNode.text

                        newLastText =
                            String.dropLeft lastOffset lastNode.text
                    in
                    [ { firstNode | text = newFirstText }, { lastNode | text = newLastText } ]


splitSelectSingle : String -> Selection -> List DocumentNode -> List DocumentNode
splitSelectSingle newId selection documentNodes =
    case List.head documentNodes of
        Nothing ->
            documentNodes

        Just node ->
            let
                minOffset =
                    min selection.anchorOffset selection.focusOffset

                maxOffset =
                    max selection.anchorOffset selection.focusOffset

                newLeftText =
                    String.left minOffset node.text

                newLeftCharacterMetadata =
                    List.take minOffset node.characterMetadata

                newRightText =
                    String.dropLeft maxOffset node.text

                newRightCharacterMetadata =
                    List.drop maxOffset node.characterMetadata
            in
            [ { node | text = newLeftText, characterMetadata = newLeftCharacterMetadata }
            , { id = newId, text = newRightText, characterMetadata = newRightCharacterMetadata, nodeType = node.nodeType }
            ]


splitBlock : Selection -> Document -> Document
splitBlock selection document =
    let
        ( before, selected, after ) =
            getSelectionBlocks selection document.nodes

        newIdCounter =
            if List.length selected == 1 then
                document.idCounter + 1

            else
                document.idCounter

        newNodes =
            case List.length selected of
                0 ->
                    document.nodes

                1 ->
                    splitSelectSingle (document.id ++ String.fromInt newIdCounter) selection selected

                _ ->
                    splitSelectedRange selection selected

        lastSelectedNodeId =
            case List.Extra.last newNodes of
                Nothing ->
                    "INVALID"

                Just node ->
                    node.id

        newSelection =
            { anchorOffset = 0
            , anchorNode = lastSelectedNodeId
            , focusOffset = 0
            , focusNode = lastSelectedNodeId
            , isCollapsed = True
            , rangeCount = 0
            , selectionType = "Caret"
            }
    in
    { document
        | nodes = before ++ newNodes ++ after
        , selection = Just newSelection
        , idCounter = document.idCounter + 1
    }



-- TODO: Implement me


splitBlockAtSelection : Document -> Document
splitBlockAtSelection document =
    case
        document.selection
    of
        Nothing ->
            document

        Just selection ->
            splitBlock selection document

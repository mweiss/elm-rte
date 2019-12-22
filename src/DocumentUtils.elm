module DocumentUtils exposing (..)

import DeleteWord
import List exposing (drop, repeat, take)
import List.Extra
import Model exposing (..)
import Regex
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



-- Run through the cases
-- Delete collapsed
-- Delete selection
-- Delete word
-- Delete line


delete : Selection -> Document -> Document
delete selection document =
    if selection.isCollapsed then
        deleteCollapsed selection.anchorNode selection.anchorOffset document

    else
        removeSelected selection document


deleteCollapsed : String -> Int -> Document -> Document
deleteCollapsed nodeId offset document =
    let
        ( before, selected, after ) =
            getSelectionSingleBlock nodeId document.nodes
    in
    case List.head selected of
        -- invalid state, TODO (can I prevent this?)
        Nothing ->
            document

        Just selectedNode ->
            let
                ( newNodeList, newSelection ) =
                    if offset == String.length selectedNode.text then
                        case List.head after of
                            Nothing ->
                                ( document.nodes, document.selection )

                            Just nextNode ->
                                let
                                    newText =
                                        selectedNode.text ++ nextNode.text

                                    newCharacterMetadata =
                                        selectedNode.characterMetadata ++ nextNode.characterMetadata
                                in
                                ( before ++ [ { nextNode | text = newText, characterMetadata = newCharacterMetadata } ] ++ List.take (List.length after - 1) after
                                , Just
                                    { anchorOffset = length selectedNode.text
                                    , anchorNode = nextNode.id
                                    , focusOffset = length selectedNode.text
                                    , focusNode = nextNode.id
                                    , isCollapsed = True
                                    , rangeCount = 0
                                    , selectionType = "Caret"
                                    }
                                )

                    else
                        ( before ++ [ removeRange offset (offset + 1) selectedNode ] ++ after
                        , Just
                            { anchorOffset = offset
                            , anchorNode = selectedNode.id
                            , focusOffset = offset
                            , focusNode = selectedNode.id
                            , isCollapsed = True
                            , rangeCount = 0
                            , selectionType = "Caret"
                            }
                        )
            in
            { document | nodes = newNodeList, selection = newSelection }


deleteWord : Selection -> Document -> Document
deleteWord selection document =
    if selection.isCollapsed then
        deleteWordCollapsed selection.anchorNode selection.anchorOffset document

    else
        removeSelected selection document


deleteWordCollapsed : String -> Int -> Document -> Document
deleteWordCollapsed nodeId offset document =
    let
        ( before, selected, after ) =
            getSelectionSingleBlock nodeId document.nodes
    in
    case List.head selected of
        -- invalid state, TODO (can I prevent this?)
        Nothing ->
            document

        Just selectedNode ->
            if offset == String.length selectedNode.text then
                deleteCollapsed nodeId offset document

            else
                let
                    matches =
                        Regex.findAtMost 1 DeleteWord.deleteWordRegex (String.dropLeft offset selectedNode.text)

                    matchOffset =
                        case List.head matches of
                            Nothing ->
                                0

                            Just match ->
                                String.length match.match + offset

                    newNode =
                        removeRange offset matchOffset selectedNode

                    newNodes =
                        before ++ [ newNode ] ++ after

                    newSelection =
                        Just
                            { anchorOffset = offset
                            , anchorNode = newNode.id
                            , focusOffset = offset
                            , focusNode = newNode.id
                            , isCollapsed = True
                            , rangeCount = 0
                            , selectionType = "Caret"
                            }
                in
                { document | nodes = newNodes, selection = newSelection }


deleteToEndOfBlock : Selection -> Document -> Document
deleteToEndOfBlock selection document =
    document


removeRange : Int -> Int -> DocumentNode -> DocumentNode
removeRange start end documentNode =
    let
        newText =
            String.left start documentNode.text ++ String.dropLeft end documentNode.text

        newCharacterMetadata =
            List.take start documentNode.characterMetadata ++ List.drop end documentNode.characterMetadata
    in
    { documentNode | text = newText, characterMetadata = newCharacterMetadata }


backspaceCollapsed : String -> Int -> Document -> Document
backspaceCollapsed nodeId offset document =
    let
        ( before, selected, after ) =
            getSelectionSingleBlock nodeId document.nodes
    in
    case List.head selected of
        -- invalid state, TODO (can I prevent this?)
        Nothing ->
            document

        Just selectedNode ->
            let
                ( newNodeList, newSelection ) =
                    if offset == 0 then
                        case List.Extra.last before of
                            Nothing ->
                                ( document.nodes, document.selection )

                            Just prevNode ->
                                let
                                    newText =
                                        prevNode.text ++ selectedNode.text

                                    newCharacterMetadata =
                                        prevNode.characterMetadata ++ selectedNode.characterMetadata
                                in
                                ( List.take (List.length before - 1) before ++ [ { prevNode | text = newText, characterMetadata = newCharacterMetadata } ] ++ after
                                , Just
                                    { anchorOffset = length prevNode.text
                                    , anchorNode = prevNode.id
                                    , focusOffset = length prevNode.text
                                    , focusNode = prevNode.id
                                    , isCollapsed = True
                                    , rangeCount = 0
                                    , selectionType = "Caret"
                                    }
                                )

                    else
                        ( before ++ [ removeRange (offset - 1) offset selectedNode ] ++ after
                        , Just
                            { anchorOffset = offset - 1
                            , anchorNode = selectedNode.id
                            , focusOffset = offset - 1
                            , focusNode = selectedNode.id
                            , isCollapsed = True
                            , rangeCount = 0
                            , selectionType = "Caret"
                            }
                        )
            in
            { document | nodes = newNodeList, selection = newSelection }


removeSelected : Selection -> Document -> Document
removeSelected selection document =
    let
        ( before, selected, after ) =
            getSelectionBlocks selection document.nodes
    in
    case List.length selected of
        -- TODO: Invalid state?
        0 ->
            document

        -- Remove selected text
        1 ->
            case List.head selected of
                -- TODO: Invalid state?
                Nothing ->
                    document

                Just selectedNode ->
                    let
                        minOffset =
                            min selection.focusOffset selection.anchorOffset

                        maxOffset =
                            max selection.focusOffset selection.anchorOffset

                        newNodes =
                            before ++ [ removeRange minOffset maxOffset selectedNode ] ++ after

                        newSelection =
                            Just
                                { anchorOffset = minOffset
                                , anchorNode = selectedNode.id
                                , focusOffset = minOffset
                                , focusNode = selectedNode.id
                                , isCollapsed = True
                                , rangeCount = 0
                                , selectionType = "Caret"
                                }
                    in
                    { document | nodes = newNodes, selection = newSelection }

        -- Remove across multiple nodes
        _ ->
            case List.head selected of
                -- TODO: Invalid state, is there a better way?
                Nothing ->
                    document

                Just selectedStart ->
                    case List.Extra.last selected of
                        Nothing ->
                            document

                        Just selectedEnd ->
                            let
                                offsetStart =
                                    if selectedStart.id == selection.anchorNode then
                                        selection.anchorOffset

                                    else
                                        selection.focusOffset

                                offsetEnd =
                                    if selectedEnd.id == selection.anchorNode then
                                        selection.anchorOffset

                                    else
                                        selection.focusOffset

                                newText =
                                    String.left offsetStart selectedStart.text ++ String.dropLeft offsetEnd selectedEnd.text

                                newCharacterMetadata =
                                    List.take offsetStart selectedStart.characterMetadata ++ List.drop offsetEnd selectedEnd.characterMetadata

                                newNode =
                                    { selectedStart | text = newText, characterMetadata = newCharacterMetadata }

                                newNodes =
                                    before ++ [ newNode ] ++ after

                                newSelection =
                                    Just
                                        { anchorOffset = offsetStart
                                        , anchorNode = newNode.id
                                        , focusOffset = offsetStart
                                        , focusNode = newNode.id
                                        , isCollapsed = True
                                        , rangeCount = 0
                                        , selectionType = "Caret"
                                        }
                            in
                            { document
                                | nodes = newNodes
                                , selection = newSelection
                            }


backspace : Selection -> Document -> Document
backspace selection document =
    if selection.isCollapsed then
        backspaceCollapsed selection.anchorNode selection.anchorOffset document

    else
        removeSelected selection document


backspaceWordCollapsed : String -> Int -> Document -> Document
backspaceWordCollapsed nodeId offset document =
    if offset == 0 then
        backspaceCollapsed nodeId offset document

    else
        let
            ( before, selected, after ) =
                getSelectionSingleBlock nodeId document.nodes
        in
        case List.head selected of
            -- invalid state, TODO (can I prevent this?)
            Nothing ->
                document

            Just selectedNode ->
                let
                    matches =
                        Regex.findAtMost 1 DeleteWord.backspaceWordRegex (String.left offset selectedNode.text)

                    matchOffset =
                        case List.head matches of
                            Nothing ->
                                0

                            Just match ->
                                match.index

                    newNode =
                        removeRange matchOffset offset selectedNode

                    newNodes =
                        before ++ [ newNode ] ++ after

                    newSelection =
                        Just
                            { anchorOffset = matchOffset
                            , anchorNode = newNode.id
                            , focusOffset = matchOffset
                            , focusNode = newNode.id
                            , isCollapsed = True
                            , rangeCount = 0
                            , selectionType = "Caret"
                            }
                in
                { document | nodes = newNodes, selection = newSelection }


backspaceWord : Selection -> Document -> Document
backspaceWord selection document =
    if selection.isCollapsed then
        backspaceWordCollapsed selection.anchorNode selection.anchorOffset document

    else
        removeSelected selection document


backspaceToBeginningOfLine : Selection -> Document -> Document
backspaceToBeginningOfLine selection document =
    document


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
                    splitSelectSingle (document.id ++ "-" ++ String.fromInt newIdCounter) selection selected

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


insertAtSelection : String -> Document -> Document
insertAtSelection value document =
    case document.selection of
        Nothing ->
            document

        Just selection ->
            let
                dataLength =
                    String.length value

                td =
                    mapDocument (insertIfSelected selection document.currentStyles value) document

                newSel =
                    { selection | focusOffset = selection.focusOffset + dataLength, anchorOffset = selection.anchorOffset + dataLength }

                newDoc =
                    { td | selection = Just newSel }
            in
            newDoc

module DocumentUtils exposing (..)

{-

   This module is a catch all for utilities for manipulating a document.  A lot of the code in this module
   could be refactored somewhere else.  Also, some of these methods with a little thought could be consolidated
   since the logic is somewhat similar (replace, delete, set all use similar traversal logic).

-}

import DeleteWord
import List exposing (repeat)
import List.Extra
import Model exposing (..)
import Regex
import String exposing (length)



{-
   Returns a tuple of (before, selected, after) given a selection object.  This is a very useful
   utility for manipulating a selection in a document.
-}


getSelectionBlocks : Selection -> List DocumentNode -> ( List DocumentNode, List DocumentNode, List DocumentNode )
getSelectionBlocks selection documentNodes =
    if selection.focusNode == selection.anchorNode then
        getSelectionSingleBlock selection.focusNode documentNodes

    else
        getSelectionRangeBlocks selection documentNodes



{-
   Finds a single node in a list of document nodes and returns a (before, selected, after) tuple
   of nodes.  selected is always of length 1 if the node exists.
-}


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



{-
   Finds a list of selected nodes in a list of document nodes and returns a (before, selected, after) tuple
   of nodes.  selected is always of length 1 if the node exists.
-}


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



{-
   Splits a block (as a result of enter or return).  This should only be called if the selected documentNodes
   is larger than 1, otherwise the logic is a bit off.
-}


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



{-
   Splits a single block (as a result of enter or return).  This is used when the selection
   is only for one block.
-}


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



{-
   Deletes the text at the given selection.  Note that delete will remove the forward character if the
   selection is collapsed.
-}


delete : Selection -> Document -> Document
delete selection document =
    if selection.isCollapsed then
        deleteCollapsed selection.anchorNode selection.anchorOffset document

    else
        removeSelected selection document



{-
   Deletes the forward most character in a block, or collapses the block if the offset is at the
   end of a block.
-}


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



{-
   Deletes the text at the given selection.  Note that delete will remove the forward most word if the
   selection is collapsed.
-}


deleteWord : Selection -> Document -> Document
deleteWord selection document =
    if selection.isCollapsed then
        deleteWordCollapsed selection.anchorNode selection.anchorOffset document

    else
        removeSelected selection document



{-
   Deletes the forward most word in a block, or collapses the block if the offset is at the
   end of a block.
-}


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



{-
   Removes the text and character metadata of a document node at the given offsets.
-}


removeRange : Int -> Int -> DocumentNode -> DocumentNode
removeRange start end documentNode =
    let
        newText =
            String.left start documentNode.text ++ String.dropLeft end documentNode.text

        newCharacterMetadata =
            List.take start documentNode.characterMetadata ++ List.drop end documentNode.characterMetadata
    in
    { documentNode | text = newText, characterMetadata = newCharacterMetadata }



{-
   Applies backspace logic to the character at the node at offset.  Merges a block with the previous one
   if the offset is at the beginning of a block.
-}


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



{-
   Replace selected updates the selection with the contents of the document node list.  Note
   that the first document node will be merged in with the selection, and if it spans multiple
   blocks, the last node in the selection will be merged in with the nodes to replace.  The document
   nodes must already have ids set on them (unless they're length 1, in which it doesn't matter
   because it will be merged into an existing node).
-}


replaceSelected : List DocumentNode -> Selection -> Document -> Document
replaceSelected replaceNodes selection document =
    let
        ( before, selected, after ) =
            getSelectionBlocks selection document.nodes
    in
    if List.length selected > 1 then
        let
            removedDocument =
                removeSelected selection document
        in
        case removedDocument.selection of
            Nothing ->
                document

            Just newSelection ->
                replaceSelected replaceNodes newSelection removedDocument

    else
        case List.head selected of
            Nothing ->
                document

            Just selectedNode ->
                let
                    minOffset =
                        min selection.focusOffset selection.anchorOffset

                    maxOffset =
                        max selection.focusOffset selection.anchorOffset

                    ( newNodes, newSelection ) =
                        replaceRange replaceNodes selectedNode minOffset maxOffset
                in
                { document | nodes = before ++ newNodes ++ after, selection = Just newSelection }



{-
   replaceRange updates a single document node with the new inserted text at the start and end
   offset.  This handles the case where the selection is within a single node.
-}


replaceRange : List DocumentNode -> DocumentNode -> Int -> Int -> ( List DocumentNode, Selection )
replaceRange newDocumentNodes selectedNode startOffset endOffset =
    let
        startText =
            String.left startOffset selectedNode.text

        endText =
            String.dropLeft endOffset selectedNode.text

        startCharacterMetadata =
            List.take startOffset selectedNode.characterMetadata

        endCharacterMetadata =
            List.drop endOffset selectedNode.characterMetadata

        defaultSelection =
            { anchorOffset = startOffset
            , anchorNode = selectedNode.id
            , focusOffset = endOffset
            , focusNode = selectedNode.id
            , isCollapsed = True
            , rangeCount = 0
            , selectionType = "Caret"
            }
    in
    case List.length newDocumentNodes of
        -- Cases:
        -- No new nodes, just return the selection
        -- New nodes are length 1, which means splicing in the text of the first block
        -- New nodes are greater than 1, which means appending the text of the first block to the selectedNode,
        -- appending the new node list, then appending the end text to the last node in the node list
        0 ->
            ( [ selectedNode ], defaultSelection )

        1 ->
            case List.head newDocumentNodes of
                Nothing ->
                    ( [ selectedNode ], defaultSelection )

                Just newDocumentNode ->
                    ( [ { selectedNode
                            | text = startText ++ newDocumentNode.text ++ endText
                            , characterMetadata = startCharacterMetadata ++ newDocumentNode.characterMetadata ++ endCharacterMetadata
                        }
                      ]
                    , { anchorOffset = length startText + length newDocumentNode.text
                      , anchorNode = selectedNode.id
                      , focusOffset = length startText + length newDocumentNode.text
                      , focusNode = selectedNode.id
                      , isCollapsed = True
                      , rangeCount = 0
                      , selectionType = "Caret"
                      }
                    )

        _ ->
            case List.head newDocumentNodes of
                Nothing ->
                    ( [ selectedNode ], defaultSelection )

                Just startNewNode ->
                    case List.Extra.last newDocumentNodes of
                        Nothing ->
                            ( [ selectedNode ], defaultSelection )

                        Just endNewNode ->
                            let
                                newStartNode =
                                    { selectedNode
                                        | text = startText ++ startNewNode.text
                                        , characterMetadata = startCharacterMetadata ++ startNewNode.characterMetadata
                                    }

                                newEndNode =
                                    { endNewNode
                                        | text = endNewNode.text ++ endText
                                        , characterMetadata = endNewNode.characterMetadata ++ endCharacterMetadata
                                    }
                            in
                            ( [ newStartNode ] ++ List.take (List.length newDocumentNodes - 2) (List.drop 1 newDocumentNodes) ++ [ newEndNode ]
                            , { anchorOffset = length endNewNode.text
                              , anchorNode = newEndNode.id
                              , focusOffset = length endNewNode.text
                              , focusNode = newEndNode.id
                              , isCollapsed = True
                              , rangeCount = 0
                              , selectionType = "Caret"
                              }
                            )


removeSelected : Selection -> Document -> Document
removeSelected selection document =
    let
        ( before, selected, after ) =
            getSelectionBlocks selection document.nodes
    in
    case List.length selected of
        -- If we have an invalid selection, then just return the document
        0 ->
            document

        -- Remove selected text
        1 ->
            case List.head selected of
                -- I don't think we can reach this case, there's probably a better way to write this...
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
                -- No way to reach this state, but I'm not sure the cleaner way
                Nothing ->
                    document

                Just selectedStart ->
                    case List.Extra.last selected of
                        -- No way to reach this state, but I'm not sure the cleaner way
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
                    ""

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
                -- No need for a new id for a single node list insertion TODO: not clear that this behavior exists
                newNodes =
                    [ { id = ""
                      , characterMetadata = repeat (String.length value) document.currentStyles
                      , text = value
                      , nodeType = "div"
                      }
                    ]

                newDocument =
                    replaceSelected newNodes selection document
            in
            newDocument


addIdToNode : DocumentNode -> Document -> ( DocumentNode, Document )
addIdToNode node document =
    ( addIdToDocumentNode document.id 0 document.idCounter node, { document | idCounter = document.idCounter + 1 } )


addIdToDocumentNode : String -> Int -> Int -> DocumentNode -> DocumentNode
addIdToDocumentNode documentId start increment documentNode =
    { documentNode | id = documentId ++ "-" ++ String.fromInt (start + increment + 1) }


addIdsToDocumentNodes : List DocumentNode -> Document -> ( List DocumentNode, Document )
addIdsToDocumentNodes nodes document =
    ( List.indexedMap (addIdToDocumentNode document.id document.idCounter) nodes
    , { document | idCounter = document.idCounter + List.length nodes }
    )


setStylesOnSelection : CharacterMetadata -> Selection -> Document -> Document
setStylesOnSelection styles selection document =
    if selection.isCollapsed then
        document

    else
        let
            ( before, selected, after ) =
                getSelectionBlocks selection document.nodes
        in
        case List.length selected of
            0 ->
                document

            1 ->
                let
                    minOffset =
                        min selection.focusOffset selection.anchorOffset

                    maxOffset =
                        max selection.focusOffset selection.anchorOffset

                    newSelected =
                        List.map
                            (\node ->
                                { node
                                    | characterMetadata =
                                        List.take minOffset node.characterMetadata
                                            ++ List.repeat (maxOffset - minOffset) styles
                                            ++ List.drop maxOffset node.characterMetadata
                                }
                            )
                            selected

                    newNodes =
                        before ++ newSelected ++ after
                in
                { document | renderCount = document.renderCount + 1, nodes = newNodes }

            _ ->
                case List.head selected of
                    -- No way to reach this state, but I'm not sure the cleaner way
                    Nothing ->
                        document

                    Just selectedStart ->
                        case List.Extra.last selected of
                            -- No way to reach this state, but I'm not sure the cleaner way
                            Nothing ->
                                document

                            Just selectedEnd ->
                                let
                                    selectedStartOffset =
                                        if selectedStart.id == selection.focusNode then
                                            selection.focusOffset

                                        else
                                            selection.anchorOffset

                                    selectedEndOffset =
                                        if selectedEnd.id == selection.focusNode then
                                            selection.focusOffset

                                        else
                                            selection.anchorOffset

                                    newSelectedStart =
                                        { selectedStart
                                            | characterMetadata =
                                                List.take selectedStartOffset selectedStart.characterMetadata
                                                    ++ List.repeat (List.length selectedStart.characterMetadata - selectedStartOffset) styles
                                        }

                                    newSelectedEnd =
                                        { selectedEnd
                                            | characterMetadata =
                                                List.repeat selectedEndOffset styles
                                                    ++ List.drop selectedEndOffset selectedEnd.characterMetadata
                                        }

                                    rest =
                                        List.drop 1 (List.take (List.length selected - 1) selected)

                                    newRest =
                                        List.map (\node -> { node | characterMetadata = List.repeat (List.length node.characterMetadata) styles }) rest
                                in
                                { document | renderCount = document.renderCount + 1, nodes = before ++ [ newSelectedStart ] ++ newRest ++ [ newSelectedEnd ] ++ after }

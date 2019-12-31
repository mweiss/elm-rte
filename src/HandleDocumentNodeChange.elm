module HandleDocumentNodeChange exposing (..)

{-
   This module handles the cust documentnodechange event which is a catch all event for spellcheck and
   autocorrect events that we only ever get notified of after they've been applied.
-}

import Array
import DocumentUtils
import Json.Decode as D
import Model exposing (..)
import Set


decodeOffsetAndMetadata : D.Decoder OffsetAndMetadata
decodeOffsetAndMetadata =
    D.map2
        OffsetAndMetadata
        (D.field "range"
            (D.map2
                Range
                (D.field "start" D.int)
                (D.field "end" D.int)
            )
        )
        (D.field "metadata" (D.list D.string))


decodeNodeInfo : D.Decoder NodeInfo
decodeNodeInfo =
    D.map5
        NodeInfo
        (D.field "id" D.string)
        (D.field "offsetsAndMetadata" (D.list decodeOffsetAndMetadata))
        (D.field "text" D.string)
        (D.field "nodeType" D.string)
        (D.field "siblings" (D.map2 Siblings (D.maybe (D.field "prev" D.string)) (D.maybe (D.field "next" D.string))))


decodeDocumentNodeChange : D.Decoder Msg
decodeDocumentNodeChange =
    D.map OnDocumentNodeChange
        (D.map3 DocumentNodeChange
            (D.at [ "detail", "updatedOrAdded" ] (D.list decodeNodeInfo))
            (D.at [ "detail", "removed" ] (D.list D.string))
            (D.at [ "detail", "forceRerender" ] D.bool)
        )



{-
   This method:
      1) Removes any nodes it needs to remove
      2) Adds or updates any nodes that need to be added
      3) Forces a rerender if necessary
-}


removeNodes : List String -> Document -> Document
removeNodes nodeIdsToRemove document =
    if List.length nodeIdsToRemove > 0 then
        forceRerender { document | nodes = List.filter (\node -> not (List.member node.id nodeIdsToRemove)) document.nodes }

    else
        document


setRange : Int -> Int -> m -> Array.Array m -> Array.Array m
setRange start end value arr =
    if end <= start then
        arr

    else
        setRange (start + 1) end value (Array.set start value arr)


createCharacterMetadata : NodeInfo -> List CharacterMetadata
createCharacterMetadata nodeInfo =
    let
        base =
            Array.repeat (String.length nodeInfo.text) emptyCharacterMetadata
    in
    Array.toList
        (List.foldl
            (\offsetAndMetadata arr ->
                let
                    metadata =
                        CharacterMetadata (Set.fromList offsetAndMetadata.metadata)
                in
                setRange offsetAndMetadata.range.start offsetAndMetadata.range.end metadata arr
            )
            base
            nodeInfo.offsetsAndMetadata
        )


insertNodeAfter : Maybe String -> DocumentNode -> Document -> Document
insertNodeAfter prevId node document =
    let
        ( nodeWithId, incrementedDocument ) =
            DocumentUtils.addIdToNode node document
    in
    case prevId of
        Nothing ->
            { incrementedDocument | nodes = [ nodeWithId ] ++ incrementedDocument.nodes }

        Just id ->
            let
                insertedNodes =
                    List.concatMap
                        (\n ->
                            if id == n.id then
                                [ n, nodeWithId ]

                            else
                                [ n ]
                        )
                        incrementedDocument.nodes
            in
            { incrementedDocument | nodes = insertedNodes }


addAndUpdateNode : NodeInfo -> Document -> Document
addAndUpdateNode nodeInfo document =
    let
        characterMetadata =
            createCharacterMetadata nodeInfo

        node =
            { characterMetadata = characterMetadata, text = nodeInfo.text, id = nodeInfo.id, nodeType = nodeInfo.nodeType }

        -- cases:
        -- 1) We cannot find the id for this node OR the id of the prevNode is the same as the
        -- id of this node.  In that case, we assign a new id to this node and insert it after prevNode.  We should force
        -- a rerender in this case.
        -- 2) We find the id of this node
        --    2a) If the text and metadata match, then we ignore this update since it matches our expectations
        --    2b) If the text and metadata do not match, then update the node and selection (TODO: how to update selection?)
    in
    if String.isEmpty nodeInfo.id || Maybe.withDefault "" nodeInfo.siblings.prev == nodeInfo.id then
        forceRerender (insertNodeAfter nodeInfo.siblings.prev node document)

    else
        replaceNode node document


replaceNode : DocumentNode -> Document -> Document
replaceNode documentNode document =
    { document
        | nodes =
            List.map
                (\x ->
                    if x.id == documentNode.id then
                        documentNode

                    else
                        x
                )
                document.nodes
    }


addAndUpdateNodes : List NodeInfo -> Document -> Document
addAndUpdateNodes nodeInfos document =
    List.foldl addAndUpdateNode document nodeInfos


forceRerender : Document -> Document
forceRerender document =
    { document | renderCount = document.renderCount + 1 }


handleDocumentNodeChange : DocumentNodeChange -> Document -> Document
handleDocumentNodeChange documentNodeChange document =
    let
        debugStatement =
            Debug.log "handleDocumentNodeChange" (List.map (\x -> x.id) document.nodes)

        removedNodesDocument =
            removeNodes documentNodeChange.removed document

        addAndUpdateNodesDocument =
            addAndUpdateNodes documentNodeChange.updatedOrAdded removedNodesDocument

        forceRerenderDocument =
            if documentNodeChange.forceRerender then
                forceRerender addAndUpdateNodesDocument

            else
                addAndUpdateNodesDocument

        debugStatement2 =
            Debug.log "handleDocumentNodeChange 2" (List.map (\x -> x.id) forceRerenderDocument.nodes)
    in
    forceRerenderDocument

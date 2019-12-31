import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';


class ElmEditor extends HTMLElement {
  constructor() {
    super();

    this._observer = new MutationObserver(this.mutationObserverCallback.bind(this))
  }

  mutationObserverCallback(mutationsList, observer) {

    let changes = gatherInformationFromMutations(mutationsList);
    console.log('mutationObserverCallback', mutationsList, changes);
    if (changes.updatedOrAdded.length === 0 &&
        !changes.forceRerender &&
        changes.removed.length === 0) {
      return;
    }
    let event = new CustomEvent("documentnodechange", {
      detail: changes
    });
    this.dispatchEvent(event)

  }

  connectedCallback() {
    this._observer.observe(this, { characterDataOldValue: true, attributeOldValue: true, attributes: true, childList: true, subtree: true, characterData: true })
  }

  disconnectedCallback() {
    this._observer.disconnect();
  }

  adpotedCallback() {
    // TODO: do something?
  }
}

customElements.define('elm-editor', ElmEditor);

/**
 * The SelectionState web component updates itself with the latest selection state, and also sets
 * the selection state whenever its attributes have been updated.  This is very useful for syncronizing
 * the selection state with what Elm thinks the selection state is, and seems to be the only way of making
 * sure the VirtualDOM has been updated already with the latest state before updating the selection state.
 */
class SelectionState extends HTMLElement {
  static get observedAttributes() {
    return ["selection"];
  }

  attributeChangedCallback(name, oldValue, selection) {
    let selectionObj = {};
    for (let pair of selection.split(",")) {
      let splitPair = pair.split("=");
      if (splitPair.length === 2) {
        selectionObj[splitPair[0]] = splitPair[1]
      }
    }
    const focusOffset = Number(selectionObj["focus-offset"]);
    const focusNode = selectionObj["focus-node"];
    const anchorOffset = Number(selectionObj["anchor-offset"]);
    const anchorNode = selectionObj["anchor-node"];

    if (focusNode && anchorNode) {
      updateSelectionToExpected({
        focusNode: focusNode,
        focusOffset: focusOffset,
        anchorOffset: anchorOffset,
        anchorNode: anchorNode
      });
    }
  }
}

customElements.define('selection-state', SelectionState);

const app = Elm.Main.init({
  node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();


/**
 * This is a helper method that will find the Selection API offset (e.g. the real DOM offset)
 * of a node given the document node and model offset.
 */
const offsetAndNode = (node, offset) => {
  let prevNode = null;
  let prevOffset = 0;
  for (let childNode of node.childNodes) {
    if (childNode.dataset && childNode.dataset.documentNodeOffset) {
      let childOffset = Number(childNode.dataset.documentNodeOffset);
      if (childOffset > offset) {
        break;
      }
      prevOffset = childOffset;
      prevNode = childNode
    }
  }

  if (prevNode && prevNode.childNodes && prevNode.childNodes[0]) {
    return {node: prevNode.childNodes[0], offset: offset - prevOffset}
  }
};

const selectDocumentNodeById = (id) => {
  return document.querySelector(`[data-document-node-id="${id}"]`)
};

/**
 * This method updates the selection state to the expected selection state.  It's used primarily
 * by the webcomponent to synchronize the selection state with what Elm thinks it should be.
 */
const updateSelectionToExpected = (expectedSelectionState) => {
  if (expectedSelectionState) {
    const data = expectedSelectionState;

    const focusNode = selectDocumentNodeById(data.focusNode);
    const anchorNode = selectDocumentNodeById(data.anchorNode);

    if (!focusNode || !anchorNode) {
      return
    }

    let focusData = offsetAndNode(focusNode, data.focusOffset);
    let anchorData = offsetAndNode(anchorNode, data.anchorOffset);

    if (!focusData || !anchorData) {
      return
    }
    const sel = window.getSelection();
    try {
      sel.setBaseAndExtent(anchorData.node, anchorData.offset, focusData.node, focusData.offset)
    } catch (e) {
      console.log("Uh oh, the selection state was incorrect!" +
          "This maybe happens because attributes are stale on the web component?",  data, focusData, anchorData);
    }
  }

};


const findDocumentNodeId = (node) => {
  let offset = 0;
  let id = "";
  let documentOffsetNode = null, documentNode = null;
  while (node && node.tagName !== "BODY") {
    if (node.dataset && node.dataset.documentNodeOffset) {
      offset = Number(node.dataset.documentNodeOffset);
      documentOffsetNode = node
    }
    if (node.dataset && node.dataset.documentNodeId) {
      id = node.dataset.documentNodeId;
      documentNode = node;
      break;
    }
    node = node.parentNode
  }
  return {offset, id, documentOffsetNode, documentNode}
};

/**
 * Whenever the selection changes, we need to tell Elm that it has updated.  Note that we translate
 * the selection so that it matches the Elm's DocumentNode model.
 */
document.addEventListener("selectionchange", (e) => {
  const selection = getSelection();
  const anchorNode = findDocumentNodeId(selection.anchorNode);
  const focusNode = findDocumentNodeId(selection.focusNode);
  app.ports.updateSelection.send({
    "anchorOffset": selection.anchorOffset + anchorNode.offset,
    "focusOffset": selection.focusOffset + focusNode.offset,
    "isCollapsed": selection.isCollapsed,
    "rangeCount": selection.rangeCount,
    "type": selection.type,
    "anchorNode": anchorNode.id,
    "focusNode": focusNode.id
  });
});

/**
 * The paste event only allows us to access the clipboard when we're handling the event, which means
 * that getting this information in Elm is quite tricky.  To work around this issue, we create
 * a new 'pastewithdata' custom event that has the clipboard data as part of the event details.
 */
document.addEventListener("paste", (e) => {
  let node = e.target;

  let documentId;
  while (node && node.tagName !== "BODY") {
    if (node.dataset && node.dataset.documentId) {
      documentId = node.dataset.documentId;
      break;
    }
    node = node.parentNode
  }

  if (!documentId) {
    return;
  }


  let pasteData = (e.clipboardData || window.clipboardData).getData('text') || "";
  let pasteDataHtml = (e.clipboardData || window.clipboardData).getData('text/html') || "";
  let newEvent = new CustomEvent("pastewithdata", {
    detail: {
      text: pasteData,
      html: pasteDataHtml
    }
  });
  e.preventDefault();
  node.dispatchEvent(newEvent)
});

const gatherInformationFromMutations = (mutationList) => {
  let processedDocumentNodes = [];
  let info = {
    updatedOrAdded: [],
    removed: [],
    forceRerender: false
  };

  const isExamined = (documentNode) => {
    for (let node of processedDocumentNodes) {
      if (node === documentNode) {
        return true;
      }
    }
    return false;
  };

  const addToUpdatedOrAdded = (documentNode) => {
    let data = deriveDataFromDocumentNode(documentNode);
    let siblings = findPreviousAndNextSiblingDocumentNodeId(documentNode);

    info.updatedOrAdded.push({
      id: (documentNode.dataset && documentNode.dataset.documentNodeId) || "",
      text: data.text,
      offsetsAndMetadata: data.offsetsAndMetadata || [],
      siblings: siblings,
      nodeType: data.nodeType
    });

    if (!(documentNode.dataset && documentNode.dataset.documentNodeId)) {
      debugger;
    }


    if (data.forceRerender) {
      info.forceRerender = true;
    }
  };

  for (let mutation of mutationList) {
    if (mutation.type === "attributes") {
      // For now, let's skip attribute changes
    } else if (mutation.type === "childList") {
      // Check if the mutation is for a document node or for a child of a document node
      for (let additionNode of mutation.addedNodes) {
        // If the added node is a document node, then add it to updatedOrAdded
        // if it's a span, actually it's the same logic...
        let documentNodeInfo = findDocumentNodeId(additionNode);

        // If there is no document node associated with this, then decide if it's a new node
        if (!documentNodeInfo.documentNode) {
          let siblings = findPreviousAndNextSiblingDocumentNodeId(additionNode);

          // Is it still part of the DOM?

          if (!siblings.next && !siblings.prev) {
            // debugger;
            // If there's no siblings, then we don't know what this is... just force a rerender
            // in this case I guess?
            if (!additionNode.parentNode || (additionNode.dataset && additionNode.dataset.rteContainer)) {
              continue;
            }
            info.forceRerender = true;
          }
          else {
            // In this case, this appears to be a new added node
            if (isExamined(additionNode)) {
              continue;
            }
            processedDocumentNodes.push(additionNode);
            addToUpdatedOrAdded(additionNode);
          }
        } else {
          if (isExamined(documentNodeInfo.documentNode)) {
            continue;
          }
          processedDocumentNodes.push(documentNodeInfo.documentNode);
          addToUpdatedOrAdded(documentNodeInfo.documentNode);
        }
      }

      // Check to see if the removed node is a document node.  If it is, check its siblings to make
      // sure it doesn't have the same id of one that we need to remove. (?? maybe I should
      // not use ids ??)
      for (let removedNode of mutation.removedNodes) {
        // If the removed node is a document node, then add it to removed nodes
        if (removedNode.dataset && removedNode.dataset.documentNodeId) {
          info.removed.push(removedNode.dataset.documentNodeId);
          continue;
        }

        // If the removed node is a child of a document node, then add it to updatedOrAdded
        let documentNodeInfo = findDocumentNodeId(removedNode);
        if (!documentNodeInfo.documentNode) {
          continue;
        }
        if (isExamined(documentNodeInfo.documentNode)) {
          continue;
        }

        processedDocumentNodes.push(documentNodeInfo.documentNode);
        addToUpdatedOrAdded(documentNodeInfo.documentNode);
      }

    } else if (mutation.type === "characterData") {
      if (isComposing) {
        continue;
      }
      let documentNodeInfo = findDocumentNodeId(mutation.target);

      if (!documentNodeInfo.documentNode) {
        // If this is a text change in a node that has no associated document node, then something
        // is wrong...
        continue;
      }

      if (isExamined(documentNodeInfo.documentNode)) {
        continue;
      }
      processedDocumentNodes.push(documentNodeInfo.documentNode);
      addToUpdatedOrAdded(documentNodeInfo.documentNode);
    } else {
      // What is this case???
    }
  }

  let nodeIds = {};
  for (let node of info.updatedOrAdded) {
    nodeIds[node.id] = true;
  }
  info.removed = info.removed.filter((id) => !nodeIds[id]);
  return info;
};

/**
 * This is a helper method that tries to figure out the text of a document node.
 */
const deriveDataFromDocumentNode = (node) => {
  if (!node) {
    console.log("WTF node is null", node);
    return {text: ""}
  }
  if (node.nodeType === Node.TEXT_NODE) {
    return {text: (node.nodeValue || "")}
  }

  let value = "";
  let offset = 0;
  let offsetsAndMetadata = [];
  let forceRerender = false;

  for (let childNode of node.childNodes) {
      let {text} = deriveDataFromDocumentNode(childNode);

      let characterMetadata = [];
      if (childNode.dataset && childNode.dataset.characterMetadata) {
        characterMetadata = childNode.dataset.characterMetadata.split(",");
      }
      let offsetAndMetadata = {range: {start: offset, end: offset + text.length}, metadata: characterMetadata};
      offsetsAndMetadata.push(offsetAndMetadata);
      offset += text.length;
      value += text
  }

  return {forceRerender, text: value, offsetsAndMetadata, nodeType: (node.dataset && node.dataset.documentNodeType || "") };
};

/**
 * This flag is used to keep track of if we're in composing state or not.  It's useful when simulating
 * the beforeinput event, as well as ignoring input events on composition.
 * @type {boolean}
 */
let isComposing = false;
document.addEventListener("compositionstart", (e) => {
  isComposing = true
});

document.addEventListener("compositionend", (e) => {
  isComposing = false;
});

const findPreviousAndNextSiblingDocumentNodeId = (documentNode) => {
  // TODO: find the previous sibling document node if it exists
  let node = documentNode.previousSibling;

  let prev = null, next = null;
  while (node) {
    if (node.dataset && node.dataset.documentNodeId) {
      prev = node.dataset.documentNodeId;
      break;
    }
    node = documentNode.previousSibling;
  }

  node = documentNode.nextSibling;
  while (node) {
    if (node.dataset && node.dataset.documentNodeId) {
      next = node.dataset.documentNodeId;
      break;
    }
    node = documentNode.nextSibling
  }

  return {prev, next};
};

// Firefox does not support beforeinput, so let's create a synthetic beforeinput event
const IS_FIREFOX = typeof InstallTrigger !== 'undefined';
if (IS_FIREFOX) {
  document.addEventListener("keypress", (e) => {
    let node = e.target;
    while (node && node.tagName !== "BODY") {
      if (node.dataset && node.dataset.documentId) {
        if (isComposing) {
          return;
        }
        let event = new InputEvent("beforeinput", {
          data: e.key,
          isComposing: isComposing,
          inputType: "insertText"
        });
        let cancelled = node.dispatchEvent(event);
        if (cancelled) {
          // e.preventDefault()
        }
        break;
      }
      node = node.parentNode
    }
  });
}

import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import {keys} from "./keys";

// Testing web component -- will probably need polymer to use correctly
class SelectionState extends HTMLElement {
  static get observedAttributes() {
    return ['focus-offset', "anchor-offset", "anchor-node",
      "focus-node", "is-collapsed", "range-count", "selection-type"];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    const focusOffset = Number(this.getAttribute("focus-offset"));
    const focusNode = this.getAttribute("focus-node");
    const anchorOffset = Number(this.getAttribute("anchor-offset"));
    const anchorNode = this.getAttribute("anchor-node");

    if (focusNode && anchorNode) {
      expectedSelectionState = {
        focusNode: focusNode,
        focusOffset: focusOffset,
        anchorOffset: anchorOffset,
        anchorNode: anchorNode
      };
      updateSelectionToExpected();
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

app.ports.tryOutgoingPort.subscribe(function(data) {
});

// TODO: use browser check library, or just don't do browser checks
// This is so bad to store this, but it's here so I can test delaying the function
let expectedSelectionState = null;


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

const updateSelectionToExpected = () => {
  if (expectedSelectionState) {
    const data = expectedSelectionState;
    expectedSelectionState = null;

    // TODO: limit this just to within the document by making it aware of the document id
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
      // TODO: look into why selection state is sometimes incorrect
      console.log("Uh oh, the selection state was incorrect!" +
          "This maybe happens because attributes are stale on the web component?");
      console.log("Test data", anchorData.node, anchorData.offset, focusData.node, focusData.offset);
      console.log("Test data2", data, focusData, anchorData);
    }
  }

};


const findDocumentNodeId = (node) => {
  let offset = 0;
  let id = "";
  while (node && node.tagName !== "BODY") {
    if (node.dataset && node.dataset.documentNodeOffset) {
      offset = Number(node.dataset.documentNodeOffset)
    }
    if (node.dataset && node.dataset.documentNodeId) {
      id = node.dataset.documentNodeId;
      break;
    }
    node = node.parentNode
  }
  return {offset, id}
};

document.addEventListener("selectionchange", (e) => {
  // updateSelectionToExpected()
  const selection = getSelection();
  const anchorNode = findDocumentNodeId(selection.anchorNode);
  const focusNode = findDocumentNodeId(selection.focusNode);

  app.ports.tryIncomingPort.send({
    "anchorOffset": selection.anchorOffset + anchorNode.offset,
    "focusOffset": selection.focusOffset + focusNode.offset,
    "isCollapsed": selection.isCollapsed,
    "rangeCount": selection.rangeCount,
    "type": selection.type,
    "anchorNode": anchorNode.id,
    "focusNode": focusNode.id
  });
});

document.addEventListener("keydown", (e) => {
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

  switch(e.key) {
    case "Backspace":
    case "Delete":
    case "Tab":
    case "Enter":
    case "Return":
      e.preventDefault();
      app.ports.tryKeyDown.send(e);
      break;
    default:
      // Do nothing.
  }
});

// Firefox does not support beforeinput, so let's create a synthetic beforeinput event
const IS_FIREFOX = typeof InstallTrigger !== 'undefined';
if (IS_FIREFOX) {
  let isComposing = false;
  document.addEventListener("compositionstart", (e) => {
    isComposing = true
  });

  document.addEventListener("compositionend", (e) => {
    isComposing = false;
  });

  document.addEventListener("keypress", (e) => {
    let node = e.target;
    while (node && node.tagName !== "BODY") {
      if (node.dataset && node.dataset.documentId) {
        let event = new InputEvent("beforeinput", {
          data: e.key,
          isComposing: isComposing,
          inputType: "insertText"
        });
        node.dispatchEvent(event);
        break;
      }
      node = node.parentNode
    }
  });
}

import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

// Testing web component -- will probably need polymer to use correctly
class SelectionState extends HTMLElement {
  static get observedAttributes() {
    return ["focus-offset", "anchor-offset", "anchor-node",
      "focus-node", "is-collapsed", "range-count", "selection-type"];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    const focusOffset = Number(this.getAttribute("focus-offset"));
    const focusNode = this.getAttribute("focus-node");
    const anchorOffset = Number(this.getAttribute("anchor-offset"));
    const anchorNode = this.getAttribute("anchor-node");
    console.log('changed', window.getSelection(), focusOffset, focusNode, anchorOffset, anchorNode);

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
  console.log(node, newEvent);
  node.dispatchEvent(newEvent)
});

const deriveTextFromDocumentNode = (node) => {
  if (node.nodeType === Node.TEXT_NODE) {
    return (node.nodeValue || "")
  }

  let value = "";
  for (let childNode of node.childNodes) {
    if (!childNode.dataset || !childNode.dataset.isEntity) {
      value += deriveTextFromDocumentNode(childNode)
    }
  }
  return value;
};

let isComposing = false;
document.addEventListener("compositionstart", (e) => {
  isComposing = true
});

document.addEventListener("compositionend", (e) => {
  isComposing = false;
});

document.addEventListener("input", (e) => {
  if (isComposing) {
    return;
  }
  let selection = window.getSelection();
  console.log('test', e);
  // If something happens on input, then we need to get the selection anchor node and derive what
  // the new text is.  This usually means that an autocomplete or spellcheck action occurred.
  // Since we really don't know what the difference is, we'll pass the new text to the editor and let
  // it resolve the difference.  Note that this probably will result in loss of style information since
  // the browser mangles the spans inside a document node, so it's unreliable to pass that information
  // to the editor.

  if (!e.target || !e.target.dataset || !e.target.dataset.documentId) {
    return;
  }

  if (!selection || !selection.anchorNode) {
    return;
  }

  const anchorNode = findDocumentNodeId(selection.anchorNode);
  let node = selectDocumentNodeById(anchorNode.id);
  console.log('node?', anchorNode.id, anchorNode, node);
  if (!node) {
    return;
  }
  let text = deriveTextFromDocumentNode(node);
  console.log('the text', text)
  let event = new CustomEvent("documentnodechange", {
    detail: {
      node: anchorNode.id, text: text
    }
  });
  console.log(event);
  e.target.dispatchEvent(event);
});

// Firefox does not support beforeinput, so let's create a synthetic beforeinput event
const IS_FIREFOX = typeof InstallTrigger !== 'undefined';
if (IS_FIREFOX) {
  document.addEventListener("keypress", (e) => {
    let node = e.target;
    while (node && node.tagName !== "BODY") {
      if (node.dataset && node.dataset.documentId) {
        let event = new InputEvent("beforeinput", {
          data: e.key,
          isComposing: isComposing,
          inputType: "insertText"
        });
        let cancelled = node.dispatchEvent(event);
        if (cancelled) {
          e.preventDefault()
        }
        break;
      }
      node = node.parentNode
    }
  });
}

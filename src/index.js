import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

/**
 * The SelectionState web component updates itself with the latest selection state, and also sets
 * the selection state whenever its attributes have been updated.  This is very useful for syncronizing
 * the selection state with what Elm thinks the selection state is, and seems to be the only way of making
 * sure the VirtualDOM has been updated already with the latest state before updating the selection state.
 */
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
      // TODO: The webcomponent updates attributes one by one, so sometimes the selection data
      // is incorrect.  One way to fix is this to put all the selection data into one attribute
      console.log("Uh oh, the selection state was incorrect!" +
          "This maybe happens because attributes are stale on the web component?",  data, focusData, anchorData);
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

/**
 * This is a helper method that tries to figure out the text of a document node.
 */
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

document.addEventListener("input", (e) => {
  if (isComposing) {
    return;
  }
  let selection = window.getSelection();
  // If something happens on input, then we need to get the selection anchor node and derive what
  // the new text is.  This usually means that an autocomplete or spellcheck action occurred.
  // Since we really don't know what the difference is, we'll pass the new text to the editor and let
  // it resolve the difference.  Note that this probably will result in loss of style information since
  // the browser mangles the spans inside a document node, so it's unreliable to pass that information
  // to the editor.  However, there's definitely a way to do this so we lose *less* style information,
  // since this will currently wipe out the entire block's style attributes.

  if (!e.target || !e.target.dataset || !e.target.dataset.documentId) {
    return;
  }

  if (!selection || !selection.anchorNode) {
    return;
  }
  registerObserver(e.target);

  const anchorNode = findDocumentNodeId(selection.anchorNode);
  let node = selectDocumentNodeById(anchorNode.id);
  if (!node) {
    return;
  }
  let text = deriveTextFromDocumentNode(node);
  let event = new CustomEvent("documentnodechange", {
    detail: {
      node: anchorNode.id, text: text
    }
  });
  e.target.dispatchEvent(event);
});

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
          e.preventDefault()
        }
        break;
      }
      node = node.parentNode
    }
  });
}


// ---
// HACK: Sometimes, a text node will be inserted at very beginning of the contenteditable
// and the VirtualDOM implementation in Elm doesn't validate that the previous DOM state is the same
// as what it thought it was before, so it'll constantly throw an exception as it tries to re-render.

// This observer tries to fix this by scanning the editor on every change and remove any top level text
// nodes if they exist.
const observers = {};
const registerObserver = (node) => {
  if (node && node.dataset && node.dataset.documentId && !observers[node.dataset.documentId]) {
    const config = { attributes: true, childList: true, subtree: true };
    const callback = function(mutationsList, observer) {
      for (let childNode of node.childNodes) {
        if (childNode.nodeType === Node.TEXT_NODE) {
          childNode.remove();
        }
      }
    };
    const observer = new MutationObserver(callback);
    observer.observe(node, config);
    observers[node.dataset.documentId] = observer;
  }
};
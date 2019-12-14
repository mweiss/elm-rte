import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

console.log('testing');


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

    if (focusNode) {
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
  console.log('try port', data)
});

// TODO: use browser check library, or just don't do browser checks
// This is so bad to store this, but it's here so I can test delaying the function
let isChromium = !! window.chrome;
let expectedSelectionState = null;

const updateSelectionToExpected = () => {
  if (expectedSelectionState) {
    const data = expectedSelectionState;
    expectedSelectionState = null;
    const focusNode = document.getElementById(data.focusNode).childNodes[0];
    const anchorNode = document.getElementById(data.anchorNode).childNodes[0];

    console.log(data);
    const sel = window.getSelection();
    sel.setBaseAndExtent(anchorNode, data.anchorOffset, focusNode, data.focusOffset)
  }

};

app.ports.updateSelectionState.subscribe(function(data) {
  /*
  console.log('ports.updateSelectionState')
  expectedSelectionState = data;
  if (isChromium) {
    updateSelectionToExpected()
  }
  */
});


const findDocumentNodeId = (node) => {
  while (node && node.tagName !== "BODY") {
    if (node.dataset && node.dataset.documentNodeId) {
      return node.dataset.documentNodeId
    }
    node = node.parentNode
  }
  return ""
};

document.addEventListener("selectionchange", (e) => {
  // updateSelectionToExpected()
  const selection = getSelection();
  const anchorNode = findDocumentNodeId(selection.anchorNode);
  const focusNode = findDocumentNodeId(selection.focusNode);


  app.ports.tryIncomingPort.send({
    "anchorOffset": selection.anchorOffset,
    "focusOffset": selection.focusOffset,
    "isCollapsed": selection.isCollapsed,
    "rangeCount": selection.rangeCount,
    "type": selection.type,
    "anchorNode": anchorNode,
    "focusNode": focusNode
  });
});

document.addEventListener("keypress", (e) => {
  e.preventDefault();
  console.log(e);
  app.ports.tryKeyPress.send(e)
});


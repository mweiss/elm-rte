import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

console.log('testing');

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


const findDocumentNodeId = (node) => {
  while (node.tagName !== "BODY") {
    if (node.dataset && node.dataset.documentNodeId) {
      return node.dataset.documentNodeId
    }
    node = node.parentNode
  }
  return ""
};

document.addEventListener("selectionchange", (e) => {
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
  console.log(e)
  app.ports.tryKeyPress.send(e)
});


import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';


var storedState = localStorage.getItem('cache');
console.log("Retrieved state: ", storedState);
var startingLowScore = storedState ? JSON.parse(storedState) : null;


var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: startingLowScore
});


// let's come back to the ports after we first sort out: flags

app.ports.cacheScore.subscribe(function(data) {
  localStorage.setItem('cache', JSON.stringify(data));
});


registerServiceWorker();

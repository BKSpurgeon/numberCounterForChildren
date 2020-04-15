import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';


var storedScore = localStorage.getItem('cache');
var startingLowScore = storedScore ? JSON.parse(storedScore) : null;


var storedBenchmark = localStorage.getItem('benchmark');
var startingBenchmark = storedBenchmark ? JSON.parse(storedBenchmark) : null;

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {startingLowScore: startingLowScore, startingBenchmark: startingBenchmark}
});


// let's come back to the ports after we first sort out: flags

app.ports.cacheScore.subscribe(function(data) {
  localStorage.setItem('cache', JSON.stringify(data));
});


app.ports.cacheBenchmark.subscribe(function(data) {
  localStorage.setItem('benchmark', JSON.stringify(data));
});



registerServiceWorker();

# tangrams-restricted
A game for organizing tangrams in the fewest number of moves using two players. More restricted than [the original version](https://github.com/errantlinguist/tangrams).

* **Since:** 2017-03-02
* **Author:** Todd Shore
* **Website:**  https://github.com/errantlinguist/tangrams-restricted
* **Licensing:** Copyright 2016 Todd Shore. Licensed for distribution under the GNU General Public License 3.0: See the file `LICENSE`.

## Requirements

* [IrisTK](http://iristk.net/) version 2017.01.27+. Currently the only way to include this is to import both this project and that of IrisTK into Eclipse and then to add the IrisTK project as a "required project on the build path".

## To do

* Add feature for measuring the similarity of two models (e.g. one seeded for "1" vs. one seeded for "2")
* Add timer which stops the game (and recording) after e.g. 15 minutes
* Add "submission\_time" attr to each game event so that *that* timestamp can be used instead of the "event\_time" attr, which is actually the time the client received the event (thereby adding a degree of lag)
* Add error handling for re-sending an event if the connection is lost (e.g. implement an "event received" event reply which the sending client waits for)
* Add SVG support so that pieces can be of arbitrary size
* Add automatic edge counting using [Canny's algorithm](https://en.wikipedia.org/wiki/Canny_edge_detector) for automatically counting the edges of a piece
* Add automatic piece generation 
# tangrams-restricted
A game for organizing tangrams in the fewest number of moves using two players. More restricted than the now-defunct original version.

* **Since:** 2017-03-02
* **Author:** Todd Shore
* **Website:**  https://github.com/errantlinguist/tangrams-restricted
* **Licensing:** Copyright 2016 Todd Shore. Licensed for distribution under the GNU General Public License 3.0: See the file `LICENSE`. Selected audio content, by Mike Koenig, is licensed under [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/).

This code was used to generate data for the following publications:

* Todd Shore and Gabriel Skantze. 2017. ["Enhancing Reference Resolution in Dialogue Using Participant Feedback"](http://www.isca-speech.org/archive/GLU_2017/abstracts/GLU2017_paper_18.html) in Giampiero Salvi and St&eacute;phane Dupont (eds.) *Proceedings of the GLU2017 International Workshop on Grounding Language Understanding*, pp. 78&ndash;82, DOI: [10.21437/GLU.2017-16](http://dx.doi.org/10.21437/GLU.2017-16).

## Requirements

* For the `client` and `server` modules, [IrisTK](http://iristk.net/) version 2017.01.27+. Currently the only way to include this is to import both this project and that of IrisTK into Eclipse and then to add the IrisTK project as a "required project on the build path" to the Eclipse projects representing both modules. [A fork of IrisTK](https://github.com/errantlinguist/IrisTK) contains the exact same classes which had to be included in this Maven project in order to get the whole thing to work together, so it's best to use that fork for the Eclipse IrisTK project to use as a dependency so that there's no possibility of getting two different versions of the same class on the classpath.

## To do

* The logged event times are broken and cannot be relied on to mean anything; Create a new `Event` attribute called e.g. `sendTime` and use that for time-syncing instead 
* Add feature for measuring the similarity of two game models, i.e. the similarity of the initial state of two different games (e.g. one seeded for "1" vs. one seeded for "2")
* Add timer which stops the game (and recording) after e.g. 15 minutes
* Add "submission\_time" attr to each game event so that *that* timestamp can be used instead of the "event\_time" attr, which is actually the time the client received the event (thereby adding a degree of lag)
* Add error handling for re-sending an event if the connection is lost (e.g. implement an "event received" event reply which the sending client waits for)
* Add SVG support so that pieces can be of arbitrary size
* Add automatic edge counting using [Canny's algorithm](https://en.wikipedia.org/wiki/Canny_edge_detector) for automatically counting the edges of a piece
* Add automatic piece generation 

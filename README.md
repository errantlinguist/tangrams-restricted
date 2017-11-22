# tangrams-restricted
A game for organizing tangrams in the fewest number of moves using two players. More restricted than the now-defunct original version.

* **Since:** 2017-03-02
* **Author:** Todd Shore
* **Website:**  https://github.com/errantlinguist/tangrams-restricted
* **Licensing:** Copyright 2016 Todd Shore. Licensed for distribution under the GNU General Public License 3.0: See the file `LICENSE`. Selected audio content, by Mike Koenig, is licensed under [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/).

This code was used to generate data for the following publications:

* Todd Shore and Gabriel Skantze. 2017. ["Enhancing Reference Resolution in Dialogue Using Participant Feedback"](http://dx.doi.org/10.21437/GLU.2017-16) in Giampiero Salvi and St&eacute;phane Dupont (eds.) [*Proceedings of the GLU2017 International Workshop on Grounding Language Understanding*](http://dx.doi.org/10.21437/GLU.2017), pp. 78&ndash;82.

## Notes

* Contains an embedded version of part of the [IrisTK](http://www.iristk.net/) platform from 8 June 2017 (revision [8440b4cbca9ae6a7231d558b7b289717a2ffb56d](https://github.com/gabriel-skantze/IrisTK/commit/8440b4cbca9ae6a7231d558b7b289717a2ffb56d)), refactored to be buildable via Maven --- see sub-modules `iristk-core`, `iristk-system`, and `iristk-broker`,
* * Likewise, IrisTK depends on Java wrappers for [PortAudio](http://www.portaudio.com/); The wrappers from version [19.6.0 `pa_stable_v190600_20161030`](https://app.assembla.com/wiki/show/portaudio/pa_stable_v190600_20161030) have been included in the sub-module `jportaudio.`

## Open issues


* The logged event times are broken and cannot be relied on to mean anything: They actually mean when the given event was *received* according to the receiving machine's own local time. Create a new `Event` attribute called e.g. `sendTime` and use that for time-syncing instead
* * Add `submission_time` attr to each game event so that *that* timestamp can be used instead of the `event_time` attr, which is actually the time the client received the event (thereby adding a degree of lag)
* Add feature for measuring the similarity of two game models, i.e. the similarity of the initial state of two different games (e.g. one seeded for "1" vs. one seeded for "2")
* Add timer which stops the game (and recording) after e.g. 15 minutes
* Add error handling for re-sending an event if the connection is lost (e.g. implement an "event received" event reply which the sending client waits for)
* Add SVG support so that pieces can be of arbitrary size
* Add automatic edge counting using [Canny's algorithm](https://en.wikipedia.org/wiki/Canny_edge_detector) for automatically counting the edges of a piece
* Add automatic piece generation 

# tangrams-restricted
A game for organizing tangrams in the fewest number of moves using two players. More restricted than [the original version](https://github.com/errantlinguist/tangrams).

* **Since:** 2017-03-02
* **Author:** Todd Shore
* **Website:**  https://github.com/errantlinguist/tangrams-restricted
* **Licensing:** Copyright 2016 Todd Shore. Licensed for distribution under the GNU General Public License 3.0: See the file `LICENSE`.

## Requirements

* [IrisTK](http://iristk.net/) version 2017.01.27+. Currently the only way to include this is to import both this project and that of IrisTK into Eclipse and then to add the IrisTK project as a "required project on the build path".

## To do

* Add "packaging" hook which puts all log files into an easily-transportable archive upon quitting the client/server
* Add feature for measuring the similarity of two models (e.g. one seeded for "1" vs. one seeded for "2")
* Add score for players based on the similarity of the current state of the model with the winning configuration. **Even better:** Calculate the absolute minimum number of moves needed to win the game and then use that similarly to how a "par" is used in golf
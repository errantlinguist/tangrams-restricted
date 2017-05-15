/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
package se.kth.speech.coin.tangrams.analysis;

import java.util.List;
import java.util.Optional;

import iristk.system.Event;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 15 May 2017
 *
 */
public final class EventDialogue {

	private final Optional<Event> lastEvent;

	private final List<Utterance> utts;

	public EventDialogue( final Optional<Event> lastEvent, final List<Utterance> utts) {
		this.lastEvent = lastEvent;
		this.utts = utts;
	}

	/**
	 * @return the lastEvent
	 */
	public Optional<Event> getLastEvent() {
		return lastEvent;
	}

	/**
	 * @return the utts
	 */
	public List<Utterance> getUtts() {
		return utts;
	}

}

/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.dialogues;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import se.kth.speech.Lists;
import se.kth.speech.coin.tangrams.iristk.GameEvent;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 15 May 2017
 *
 */
public final class EventDialogue {

	private final List<GameEvent> events;

	private final List<Utterance> utts;

	public EventDialogue(final List<GameEvent> events, final List<Utterance> utts) {
		this.events = Collections.unmodifiableList(events);
		this.utts = Collections.unmodifiableList(utts);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof EventDialogue)) {
			return false;
		}
		final EventDialogue other = (EventDialogue) obj;
		if (events == null) {
			if (other.events != null) {
				return false;
			}
		} else if (!events.equals(other.events)) {
			return false;
		}
		if (utts == null) {
			if (other.utts != null) {
				return false;
			}
		} else if (!utts.equals(other.utts)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the events
	 */
	public List<GameEvent> getEvents() {
		return events;
	}

	public Optional<GameEvent> getFirstEvent() {
		return Lists.getFirstElement(events);
	}

	public Optional<Utterance> getFirstUtterance() {
		return Lists.getFirstElement(utts);
	}

	public Optional<GameEvent> getLastEvent() {
		return Lists.getLastElement(events);
	}

	public Optional<Utterance> getLastUtterance() {
		return Lists.getLastElement(utts);
	}

	/**
	 * @return the utts
	 */
	public List<Utterance> getUtterances() {
		return utts;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (events == null ? 0 : events.hashCode());
		result = prime * result + (utts == null ? 0 : utts.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(64 * (events.size() + utts.size() + 1));
		builder.append("EventDialogue [events=");
		builder.append(events);
		builder.append(", utts=");
		builder.append(utts);
		builder.append(']');
		return builder.toString();
	}

}

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
package se.kth.speech.coin.tangrams.iristk;

import java.util.Collection;
import java.util.EnumSet;
import java.util.function.Predicate;

import iristk.system.Event;

public final class EventTypeMatcher implements Predicate<Event> {

	private final Collection<GameManagementEvent> acceptedTypes;

	public EventTypeMatcher(final Collection<GameManagementEvent> acceptedTypes) {
		this.acceptedTypes = acceptedTypes;
	}

	public EventTypeMatcher(final GameManagementEvent firstAcceptedType,
			final GameManagementEvent... nextAcceptedTypes) {
		this(EnumSet.of(firstAcceptedType, nextAcceptedTypes));
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
		if (!(obj instanceof EventTypeMatcher)) {
			return false;
		}
		final EventTypeMatcher other = (EventTypeMatcher) obj;
		if (acceptedTypes == null) {
			if (other.acceptedTypes != null) {
				return false;
			}
		} else if (!acceptedTypes.equals(other.acceptedTypes)) {
			return false;
		}
		return true;
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
		result = prime * result + (acceptedTypes == null ? 0 : acceptedTypes.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Predicate#test(java.lang.Object)
	 */
	@Override
	public boolean test(final Event event) {
		final GameManagementEvent eventType = GameManagementEvent.getEventType(event);
		return acceptedTypes.contains(eventType);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(64);
		builder.append("EventTypeMatcher [acceptedTypes=");
		builder.append(acceptedTypes);
		builder.append(']');
		return builder.toString();
	}
}
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
package se.kth.speech.coin.tangrams.analysis.dialogues;

import java.time.LocalDateTime;
import java.util.function.Predicate;

import se.kth.speech.TimestampArithmetic;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Aug 2017
 *
 */
public final class ClosedTemporalIntervalUtteranceFilter implements Predicate<Utterance> {

	private final LocalDateTime end;

	private final LocalDateTime start;

	public ClosedTemporalIntervalUtteranceFilter(final LocalDateTime start, final LocalDateTime end) {
		if (start.isAfter(end)) {
			throw new IllegalArgumentException("Start time cannot be after end time.");
		}
		this.start = start;
		this.end = end;
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
		if (!(obj instanceof ClosedTemporalIntervalUtteranceFilter)) {
			return false;
		}
		final ClosedTemporalIntervalUtteranceFilter other = (ClosedTemporalIntervalUtteranceFilter) obj;
		if (end == null) {
			if (other.end != null) {
				return false;
			}
		} else if (!end.equals(other.end)) {
			return false;
		}
		if (start == null) {
			if (other.start != null) {
				return false;
			}
		} else if (!start.equals(other.start)) {
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
		result = prime * result + (end == null ? 0 : end.hashCode());
		result = prime * result + (start == null ? 0 : start.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Predicate#test(java.lang.Object)
	 */
	@Override
	public boolean test(final Utterance utt) {
		final LocalDateTime uttStart = TimestampArithmetic.createOffsetTimestamp(start, utt.getStartTime());
		final boolean uttStartedWithinTimeframe = uttStart.compareTo(start) >= 0;

		final boolean result;
		if (uttStartedWithinTimeframe) {
			final LocalDateTime uttEnd = TimestampArithmetic.createOffsetTimestamp(start, utt.getEndTime());
			result = uttEnd.compareTo(end) <= 0;
		} else {
			result = false;
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("ClosedTemporalIntervalUtteranceFilter [start=");
		builder.append(start);
		builder.append(", end=");
		builder.append(end);
		builder.append(']');
		return builder.toString();
	}

}

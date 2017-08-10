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

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public final class Utterance implements Comparable<Utterance> {

	private static final Comparator<Utterance> NATURAL_COMPARATOR = Comparator.comparingDouble(Utterance::getStartTime)
			.thenComparingDouble(Utterance::getEndTime).thenComparingInt(utt -> utt.getTokens().size())
			.thenComparing(Utterance::getSpeakerId).thenComparing(Utterance::getSegmentId);

	private static final Collector<CharSequence, ?, String> WORD_JOINER = Collectors.joining(" ");

	private final float endTime;

	private final String segmentId;

	private final String speakerId;

	private final float startTime;

	private final List<String> tokens;

	private final String tokenStr;

	public Utterance(final String segmentId, final String speakerId, final List<String> tokens, final float startTime,
			final float endTime) {
		if (startTime > endTime) {
			throw new IllegalArgumentException("Start time is greater than end time.");
		}
		this.segmentId = segmentId;
		this.speakerId = speakerId;
		this.tokens = tokens;
		tokenStr = tokens.stream().collect(WORD_JOINER);
		this.startTime = startTime;
		this.endTime = endTime;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(final Utterance o) {
		return NATURAL_COMPARATOR.compare(this, o);
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
		if (!(obj instanceof Utterance)) {
			return false;
		}
		final Utterance other = (Utterance) obj;
		if (Float.floatToIntBits(endTime) != Float.floatToIntBits(other.endTime)) {
			return false;
		}
		if (segmentId == null) {
			if (other.segmentId != null) {
				return false;
			}
		} else if (!segmentId.equals(other.segmentId)) {
			return false;
		}
		if (speakerId == null) {
			if (other.speakerId != null) {
				return false;
			}
		} else if (!speakerId.equals(other.speakerId)) {
			return false;
		}
		if (Float.floatToIntBits(startTime) != Float.floatToIntBits(other.startTime)) {
			return false;
		}
		if (tokens == null) {
			if (other.tokens != null) {
				return false;
			}
		} else if (!tokens.equals(other.tokens)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the endTime
	 */
	public float getEndTime() {
		return endTime;
	}

	/**
	 * @return the segmentId
	 */
	public String getSegmentId() {
		return segmentId;
	}

	/**
	 * @return the speakerId
	 */
	public String getSpeakerId() {
		return speakerId;
	}

	/**
	 * @return the startTime
	 */
	public float getStartTime() {
		return startTime;
	}

	/**
	 * @return the tokens
	 */
	public List<String> getTokens() {
		return tokens;
	}

	/**
	 * @return the tokenStr
	 */
	public String getTokenStr() {
		return tokenStr;
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
		result = prime * result + Float.floatToIntBits(endTime);
		result = prime * result + (segmentId == null ? 0 : segmentId.hashCode());
		result = prime * result + (speakerId == null ? 0 : speakerId.hashCode());
		result = prime * result + Float.floatToIntBits(startTime);
		result = prime * result + (tokens == null ? 0 : tokens.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(96 + 4 * tokens.size());
		builder.append("Utterance [startTime=");
		builder.append(startTime);
		builder.append(", endTime=");
		builder.append(endTime);
		builder.append(", speakerId=");
		builder.append(speakerId);
		builder.append(", segmentId=");
		builder.append(segmentId);
		builder.append(", tokens=");
		builder.append(tokens);
		builder.append(']');
		return builder.toString();
	}
}
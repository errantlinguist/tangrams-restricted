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

public final class Utterance {

	private final double endTime;

	private final String segmentId;

	private final double startTime;

	private final List<String> tokens;

	public Utterance(final String segmentId, final List<String> tokens, final double startTime, final double endTime) {
		if (startTime > endTime) {
			throw new IllegalArgumentException("Start time is greater than end time.");
		}
		this.segmentId = segmentId;
		this.tokens = tokens;
		this.startTime = startTime;
		this.endTime = endTime;
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
		if (Double.doubleToLongBits(endTime) != Double.doubleToLongBits(other.endTime)) {
			return false;
		}
		if (segmentId == null) {
			if (other.segmentId != null) {
				return false;
			}
		} else if (!segmentId.equals(other.segmentId)) {
			return false;
		}
		if (Double.doubleToLongBits(startTime) != Double.doubleToLongBits(other.startTime)) {
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
	public double getEndTime() {
		return endTime;
	}

	/**
	 * @return the segmentId
	 */
	public String getSegmentId() {
		return segmentId;
	}

	/**
	 * @return the startTime
	 */
	public double getStartTime() {
		return startTime;
	}

	/**
	 * @return the tokens
	 */
	public List<String> getTokens() {
		return tokens;
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
		long temp;
		temp = Double.doubleToLongBits(endTime);
		result = prime * result + (int) (temp ^ temp >>> 32);
		result = prime * result + (segmentId == null ? 0 : segmentId.hashCode());
		temp = Double.doubleToLongBits(startTime);
		result = prime * result + (int) (temp ^ temp >>> 32);
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
		final StringBuilder builder = new StringBuilder();
		builder.append("Utterance [segmentId=");
		builder.append(segmentId);
		builder.append(", startTime=");
		builder.append(startTime);
		builder.append(", endTime=");
		builder.append(endTime);
		builder.append(", tokens=");
		builder.append(tokens);
		builder.append(']');
		return builder.toString();
	}
}
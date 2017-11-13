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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import it.unimi.dsi.fastutil.ints.Int2IntMap;
import it.unimi.dsi.fastutil.ints.Int2IntMaps;
import it.unimi.dsi.fastutil.ints.Int2IntOpenHashMap;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;

public final class SessionTestResults {

	private final List<Entry<EventDialogue, EventDialogueTestResults>> diagTestResults;

	private final Int2IntMap goldStdReferentIdCounts;

	public SessionTestResults(final int expectedDiagTestCount) {
		diagTestResults = new ArrayList<>(expectedDiagTestCount);
		goldStdReferentIdCounts = new Int2IntOpenHashMap(expectedDiagTestCount);
		goldStdReferentIdCounts.defaultReturnValue(0);
	}

	public void add(final Entry<EventDialogue, EventDialogueTestResults> diagTestResults) {
		this.diagTestResults.add(diagTestResults);
		final int refId = diagTestResults.getValue().getGoldStandardReferentId();
		goldStdReferentIdCounts.put(refId, goldStdReferentIdCounts.get(refId) + 1);
	}

	public void add(final SessionTestResults other) {
		other.diagTestResults.forEach(this::add);
		diagTestResults.addAll(other.diagTestResults);
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
		if (!(obj instanceof SessionTestResults)) {
			return false;
		}
		final SessionTestResults other = (SessionTestResults) obj;
		if (diagTestResults == null) {
			if (other.diagTestResults != null) {
				return false;
			}
		} else if (!diagTestResults.equals(other.diagTestResults)) {
			return false;
		}
		if (goldStdReferentIdCounts == null) {
			if (other.goldStdReferentIdCounts != null) {
				return false;
			}
		} else if (!goldStdReferentIdCounts.equals(other.goldStdReferentIdCounts)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the diagResults
	 */
	public List<Entry<EventDialogue, EventDialogueTestResults>> getDialogueTestResults() {
		return diagTestResults;
	}

	/**
	 * @return the goldStdReferentIdCounts. <strong>NOTE:</strong> This has no
	 *         meaning across sessions because entity IDs are not stable across
	 *         sessions, i.e.&nbsp;the entity with ID <code>2</code> in one game
	 *         has nothing to do with the entity with ID <code>2</code> in
	 *         another game.
	 */
	public Int2IntMap getGoldStdReferentIdCounts() {
		return Int2IntMaps.unmodifiable(goldStdReferentIdCounts);
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
		result = prime * result + (diagTestResults == null ? 0 : diagTestResults.hashCode());
		result = prime * result + (goldStdReferentIdCounts == null ? 0 : goldStdReferentIdCounts.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(1024);
		builder.append("SessionTestResults [diagTestResults=");
		builder.append(diagTestResults);
		builder.append(", goldStdReferentIdCounts=");
		builder.append(goldStdReferentIdCounts);
		builder.append("]");
		return builder.toString();
	}

}
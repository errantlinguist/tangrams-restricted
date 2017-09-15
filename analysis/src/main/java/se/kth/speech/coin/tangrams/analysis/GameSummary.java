/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;

import iristk.system.Event;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

final class GameSummary {

	private final long completedRoundCount;

	private final Duration duration;

	private final long uttCount;

	GameSummary(final GameHistory history, final List<EventDialogue> diags) {
		// Remove one from count because the last round of the game is never
		// actually finished
		// TODO: Remove this once one adding the remove-last-round functionality
		// to the EventDialogue factory
		final int lastDiagIdx = diags.size() - 0;
		final List<EventDialogue> completedDiags = diags.subList(0, lastDiagIdx);

		completedRoundCount = completedDiags.size();
		uttCount = completedDiags.stream().map(EventDialogue::getUtterances).flatMap(List::stream).count();
		assert uttCount > 0;

		final LocalDateTime gameStart = history.getStartTime();
		final Event gameOverEvent = diags.get(lastDiagIdx).getFirstEvent().get();
		final LocalDateTime lastTurnRequestTime = EventTimes.parseEventTime(gameOverEvent.getTime());
		duration = Duration.between(gameStart, lastTurnRequestTime);
	}

	GameSummary(final long roundCount, final Duration duration, final long uttCount) {
		completedRoundCount = roundCount;
		this.duration = duration;
		this.uttCount = uttCount;
	}

	/**
	 * @return the completedRoundCount
	 */
	public long getCompletedRoundCount() {
		return completedRoundCount;
	}

	/**
	 * @return the duration
	 */
	public Duration getDuration() {
		return duration;
	}

	/**
	 * @return the uttCount
	 */
	public long getUttCount() {
		return uttCount;
	}
}
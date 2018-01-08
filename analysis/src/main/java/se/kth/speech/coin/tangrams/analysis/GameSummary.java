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
package se.kth.speech.coin.tangrams.analysis;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.function.Supplier;
import java.util.stream.Stream;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.iristk.GameEvent;

final class GameSummary {

	private static int findIncompleteDialogueTailStartIndex(final List<EventDialogue> diags) {
		final int prevIdx = diags.size();
		int result = prevIdx;
		final ListIterator<EventDialogue> diagIter = diags.listIterator(diags.size());
		while (diagIter.hasPrevious()) {
			final EventDialogue prevDiag = diagIter.previous();
			if (prevDiag.getUtterances().isEmpty()) {
				result = prevIdx;
				break;
			}
		}
		return result;
	}

	private static Stream<Utterance> getUtterances(final Collection<EventDialogue> diags) {
		return diags.stream().map(EventDialogue::getUtterances).flatMap(List::stream);
	}

	private final long completedRoundCount;

	private final Duration duration;

	private final Supplier<Stream<Utterance>> uttsGetter;

	GameSummary(final GameHistory history, List<EventDialogue> diags) {
		// Remove pre-game dialogue if present
		if (diags.get(0).getEvents().isEmpty()){
			diags = diags.subList(1, diags.size());
		}
		
		final int lastDiagEndIdx = findIncompleteDialogueTailStartIndex(diags);
		final List<EventDialogue> completedDiags;
		final GameEvent gameOverEvent;
		if (lastDiagEndIdx < diags.size()) {
			completedDiags = diags.subList(0, lastDiagEndIdx);
			final EventDialogue firstIncompleteDiag = diags.get(lastDiagEndIdx);
			gameOverEvent = firstIncompleteDiag.getFirstEvent().get();
		} else {
			completedDiags = diags;
			final EventDialogue lastCompletedDiag = completedDiags.listIterator(completedDiags.size()).previous();
			gameOverEvent = lastCompletedDiag.getLastEvent().get();
		}
		completedRoundCount = completedDiags.size();
		uttsGetter = () -> getUtterances(completedDiags);

		final LocalDateTime gameStart = history.getStartTime();
		final LocalDateTime lastTurnRequestTime = gameOverEvent.getTime();
		duration = Duration.between(gameStart, lastTurnRequestTime);
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

	public Stream<Utterance> getUtterances() {
		return uttsGetter.get();
	}
}
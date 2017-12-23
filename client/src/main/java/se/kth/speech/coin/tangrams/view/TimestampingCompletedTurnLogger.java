/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams.view;

import java.awt.Component;
import java.text.DateFormat;
import java.util.Date;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

import se.kth.speech.coin.tangrams.game.Turn;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Jan 2017
 *
 */
final class TimestampingCompletedTurnLogger implements BiConsumer<Component, Turn> {

	private final Supplier<? extends DateFormat> dateFormatSupplier;

	private final BiConsumer<? super Component, ? super String> screenshotLogger;

	TimestampingCompletedTurnLogger(final BiConsumer<? super Component, ? super String> screenshotLogger,
			final Supplier<? extends DateFormat> dateFormatSupplier) {
		this.screenshotLogger = screenshotLogger;
		this.dateFormatSupplier = dateFormatSupplier;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.BiConsumer#accept(java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public void accept(final Component view, final Turn turn) {
		final int seqOrdinality = turn.getSequenceOrdinality();
		// The screenshot is actually of the next round in the game
		final int nextRoundId = seqOrdinality + 1;
		// Get the exact time of the screenshot because it's possible the OS
		// puts a slightly-different creation date on the file saved
		final String timestamp = dateFormatSupplier.get().format(new Date());
		final String filenamePrefix = "round-" + Integer.toString(nextRoundId) + "-" + timestamp;
		screenshotLogger.accept(view, filenamePrefix);
	}

}

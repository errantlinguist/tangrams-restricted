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
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.function.BiConsumer;

import se.kth.speech.coin.tangrams.iristk.events.Selection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Jan 2017
 *
 */
final class TimestampingSelectionLogger implements BiConsumer<Component, Selection> {

	private static final ThreadLocal<SimpleDateFormat> TIME_FORMAT = new ThreadLocal<SimpleDateFormat>() {

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.ThreadLocal#initialValue()
		 */
		@Override
		protected SimpleDateFormat initialValue() {
			return new SimpleDateFormat("HHmmssSSS");
		}

	};

	private final BiConsumer<? super Component, ? super String> screenshotLogger;

	TimestampingSelectionLogger(final BiConsumer<? super Component, ? super String> screenshotLogger) {
		this.screenshotLogger = screenshotLogger;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.BiConsumer#accept(java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public void accept(final Component view, final Selection selection) {
		// Get the exact time of the screenshot because it's possible the OS
		// puts a slightly-different creation date on the file saved
		final String timestamp = TIME_FORMAT.get().format(new Date());
		final String filenamePrefix = "selection-" + timestamp;
		screenshotLogger.accept(view, filenamePrefix);

	}

}

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
package se.kth.speech.io;

import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import javax.sound.sampled.Line;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class LineFutureCloser implements Runnable {

	private static final Logger LOGGER = LoggerFactory.getLogger(LineFutureCloser.class);

	private final CompletableFuture<? extends Line> lineFuture;

	public LineFutureCloser(final CompletableFuture<? extends Line> lineFuture) {
		this.lineFuture = lineFuture;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		LOGGER.info("Cleaning up audio resources.");
		final boolean wasCancelled = lineFuture.cancel(true);
		LOGGER.debug("Audio data line future was cancelled?: {}", wasCancelled);
		try {
			final Line line = lineFuture.get();
			LOGGER.debug("Closing audio data line: {}", line);
			LOGGER.debug("Line implementation class is \"{}\".", line.getClass().getName());
			// FIXME: Cannot close the Clip/Line on Linux with
			// PulseAudio
			line.close();
			LOGGER.debug("Closed audio data line: {}", line);
		} catch (final CancellationException e) {
			LOGGER.debug("Audio data line future was cencelled before completion; No resources to clean up.");
		} catch (final InterruptedException e) {
			LOGGER.debug("Audio data line future was interrupted before completion.");
		} catch (final ExecutionException e) {
			LOGGER.error("An error occured while trying to close the audio data line,", e);
		}
	}
}
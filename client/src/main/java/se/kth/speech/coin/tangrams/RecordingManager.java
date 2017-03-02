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
package se.kth.speech.coin.tangrams;

import java.io.File;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.audio.AudioRecorder;
import iristk.audio.Microphone;
import iristk.system.InitializationException;
import se.kth.speech.OneOffRunnable;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 13 Jan 2017
 *
 */
final class RecordingManager {

	private static final Logger LOGGER = LoggerFactory.getLogger(RecordingManager.class);

	private final Supplier<? extends File> outdirSupplier;

	private final AudioRecorder recorder;

	private transient final Runnable stopper;

	public RecordingManager(final Supplier<? extends File> outdirSupplier) throws InitializationException {
		this.outdirSupplier = outdirSupplier;
		final Microphone mic = new Microphone();
		recorder = new AudioRecorder(mic);
		stopper = new OneOffRunnable(() -> {
			LOGGER.info("Stopping recording.");
			recorder.stopRecording();
			mic.stop();
		});
	}

	/**
	 * @return the stopper
	 */
	public Runnable getStopper() {
		return stopper;
	}

	public void start(final String playerId) {
		final File logDir = outdirSupplier.get();
		final String outfileName = playerId + ".wav";
		final File outpath = new File(logDir, outfileName);
		recorder.startRecording(outpath);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#finalize()
	 */
	@Override
	protected void finalize() throws Throwable {
		try {
			stopper.run();
		} finally {
			super.finalize();
		}
	}

}

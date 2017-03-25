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

import java.awt.event.WindowEvent;
import java.util.function.Consumer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.view.ConnectionStatusFrame;

final class SuccessfulConnectionHook implements Runnable {

	private static final Logger LOGGER = LoggerFactory.getLogger(SuccessfulConnectionHook.class);

	private final ConnectionStatusFrame connectionStatusView;

	private final String playerId;

	private final Consumer<? super String> recordingManager;

	SuccessfulConnectionHook(final ConnectionStatusFrame connectionStatusView,
			final Consumer<? super String> recordingManager, final String playerId) {
		this.connectionStatusView = connectionStatusView;
		this.recordingManager = recordingManager;
		this.playerId = playerId;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		LOGGER.debug("Closing connection status view.");
		connectionStatusView.setStatus(ConnectionStatusFrame.Status.CONNECTED);
		// http://stackoverflow.com/a/1235994/1391325
		connectionStatusView.dispatchEvent(new WindowEvent(connectionStatusView, WindowEvent.WINDOW_CLOSING));
		// Create recording output file and start
		// recording
		recordingManager.accept(playerId);
	}
}
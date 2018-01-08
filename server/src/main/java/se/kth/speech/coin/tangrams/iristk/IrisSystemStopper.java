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
package se.kth.speech.coin.tangrams.iristk;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.IrisSystem;
import se.kth.speech.OneOffRunnable;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 7 Dec 2016
 *
 */
public final class IrisSystemStopper implements Runnable {

	private static final Logger LOGGER = LoggerFactory.getLogger(IrisSystemStopper.class);

	private final Runnable delegate;

	public IrisSystemStopper(final IrisSystem system) {
		delegate = new OneOffRunnable(() -> {
			LOGGER.info("Shutting down IrisTK broker client \"{}\".", system.getName());
			system.stop();
		});
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		delegate.run();
	}

}

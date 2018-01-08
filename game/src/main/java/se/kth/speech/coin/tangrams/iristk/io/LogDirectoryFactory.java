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
package se.kth.speech.coin.tangrams.iristk.io;

import java.io.File;
import java.util.Date;
import java.util.function.Supplier;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 17 Jan 2017
 *
 */
public final class LogDirectoryFactory implements Supplier<File> {

	private final File outdir;

	private final Date systemLoggingStartTime;

	public LogDirectoryFactory(final File outdir, final Date systemLoggingStartTime) {
		this.outdir = outdir;
		this.systemLoggingStartTime = systemLoggingStartTime;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public File get() {
		final File dateSubdir = new File(outdir, LoggingFormats.DATE_FORMAT.get().format(systemLoggingStartTime));
		return new File(dateSubdir, LoggingFormats.TIME_FORMAT.get().format(systemLoggingStartTime));
	}

}

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
package se.kth.speech.coin.tangrams.iristk.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.Properties;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 3, 2017
 *
 */
public final class HatIO {

	private static final String ANNOT_CONTEXT;

	private static volatile Reference<JAXBContext> jc = new SoftReference<>(null);

	static {
		try (InputStream is = HatIO.class.getResourceAsStream("/hat.properties")) {
			final Properties props = new Properties();
			props.load(is);
			ANNOT_CONTEXT = props.getProperty("hat.package");
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	public static JAXBContext fetchContext() {
		JAXBContext result = jc.get();
		if (result == null) {
			synchronized (jc) {
				result = jc.get();
				if (result == null) {
					try {
						result = JAXBContext.newInstance(ANNOT_CONTEXT);
						jc = new SoftReference<>(result);
					} catch (final JAXBException e) {
						throw new AssertionError(e);
					}
				}
			}
		}
		return result;
	}

	private HatIO() {
	}

}

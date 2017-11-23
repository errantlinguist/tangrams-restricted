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
package se.kth.speech.higgins.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import se.kth.speech.higgins._2005.annotation.Annotation;
import se.kth.speech.io.RuntimeJAXBException;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 3, 2017
 *
 */
public final class HatIO {

	private static final String ANNOT_CONTEXT;

	private static final ThreadLocal<Unmarshaller> HAT_UNMARSHALLER = new ThreadLocal<Unmarshaller>() {

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.ThreadLocal#initialValue()
		 */
		@Override
		protected Unmarshaller initialValue() {
			try {
				return HatIO.fetchContext().createUnmarshaller();
			} catch (final JAXBException e) {
				throw new RuntimeJAXBException(e);
			}
		}

	};

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
			synchronized (HatIO.class) {
				result = jc.get();
				if (result == null) {
					try {
						result = JAXBContext.newInstance(ANNOT_CONTEXT);
						jc = new SoftReference<>(result);
					} catch (final JAXBException e) {
						throw new RuntimeJAXBException(e);
					}
				}
			}
		}
		return result;
	}

	public static Unmarshaller fetchUnmarshaller() {
		return HAT_UNMARSHALLER.get();
	}

	public static Annotation readAnnotation(final Path hatInfilePath) throws JAXBException, IOException {
		try (InputStream instream = Files.newInputStream(hatInfilePath)) {
			return (Annotation) fetchUnmarshaller().unmarshal(instream);
		}
	}

	private HatIO() {
	}

}

/*
 *  This file is part of se.kth.speech.coin.tangrams.client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
public final class ClasspathDirResourceLocatorMapFactory<K, M extends Map<K, URL>> implements Function<String, M> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ClasspathDirResourceLocatorMapFactory.class);

	/**
	 *
	 * @param resUrl
	 * @param instream
	 * @throws IOException
	 *
	 * @see <a href="https://bugs.openjdk.java.net/browse/JDK-8080094">OpenJDK
	 *      bug report</a>
	 */
	private static void close(final URL resUrl, final InputStream instream) throws IOException {
		final URLConnection connection = resUrl.openConnection();
		if (connection instanceof JarURLConnection) {
			final JarURLConnection jar = (JarURLConnection) connection;
			if (jar.getUseCaches()) {
				jar.getJarFile().close();
			}
		} else {
			instream.close();
		}
	}

	private final Function<? super String, ? extends K> fileResourceNameFactory;

	private final Predicate<? super String> pathFilter;

	private final Function<? super String, ? extends URL> resourceUrlFactory;

	private final Supplier<? extends M> supplier;

	public ClasspathDirResourceLocatorMapFactory(final Class<?> loadingClass, final Supplier<? extends M> supplier,
			final Predicate<? super String> pathFilter,
			final Function<? super String, ? extends K> fileResourceNameFactory) {
		this(loadingClass::getResource, supplier, pathFilter, fileResourceNameFactory);
	}

	public ClasspathDirResourceLocatorMapFactory(final Function<? super String, ? extends URL> resourceUrlFactory,
			final Supplier<? extends M> supplier, final Predicate<? super String> pathFilter,
			final Function<? super String, ? extends K> fileResourceNameFactory) {
		this.resourceUrlFactory = resourceUrlFactory;
		this.supplier = supplier;
		this.pathFilter = pathFilter;
		this.fileResourceNameFactory = fileResourceNameFactory;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public M apply(final String dirToMap) {
		LOGGER.debug("Creating map of resource dir \"{}\".", dirToMap);
		final URL dirUrl = resourceUrlFactory.apply(dirToMap);
		LOGGER.debug("Reading URL \"{}\".", dirUrl);

		final M result = supplier.get();

		try {
			InputStream dirInstream = null;
			try {
				dirInstream = dirUrl.openStream();
				final BufferedReader br = new BufferedReader(new InputStreamReader(dirInstream));
				for (String line = br.readLine(); line != null; line = br.readLine()) {
					if (pathFilter.test(line)) {
						final K fileResourceName = fileResourceNameFactory.apply(line);
						final URL resource = resourceUrlFactory.apply(dirToMap + "/" + line);
						result.put(fileResourceName, resource);
					}
				}
			} finally {
				close(dirUrl, dirInstream);
			}
		} catch (final IOException e) {
			final UncheckedIOException ne = new UncheckedIOException(e);
			LOGGER.error("Caught a checked {}; Re-throwing as an unchecked {}.", e.getClass().getSimpleName(),
					ne.getClass().getSimpleName());
			throw ne;
		}

		return result;
	}

}

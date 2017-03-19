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
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.net.URL;
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
public final class ClasspathDirResourceLocatorMapFactory<K, R extends Map<K, URL>> implements Function<String, R> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ClasspathDirResourceLocatorMapFactory.class);

	private final Function<? super String, ? extends K> fileResourceNameFactory;

	private final Class<?> loadingClass;

	private final Predicate<? super String> pathFilter;

	private final Supplier<? extends R> supplier;

	public ClasspathDirResourceLocatorMapFactory(final Class<?> loadingClass, final Supplier<? extends R> supplier,
			final Predicate<? super String> pathFilter,
			final Function<? super String, ? extends K> fileResourceNameFactory) {
		this.loadingClass = loadingClass;
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
	public R apply(final String dirToMap) {
		final R result = supplier.get();

		try (BufferedReader br = new BufferedReader(
				new InputStreamReader(loadingClass.getResourceAsStream(dirToMap)))) {
			for (String line = br.readLine(); line != null; line = br.readLine()) {
				if (pathFilter.test(line)) {
					final K fileResourceName = fileResourceNameFactory.apply(line);
					final URL resource = loadingClass.getResource(dirToMap + "/" + line);
					result.put(fileResourceName, resource);
				}
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

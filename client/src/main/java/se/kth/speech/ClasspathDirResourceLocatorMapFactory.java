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
package se.kth.speech;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
public final class ClasspathDirResourceLocatorMapFactory<T extends Map<String, URL>> implements Function<String, T> {

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4546093/1391325">StackOverflow</a>
	 */
	private static final Pattern FILE_EXT_SPLITTING_PATTERN = Pattern.compile("\\.(?!.*\\.)");

	private static final Logger LOGGER = LoggerFactory.getLogger(ClasspathDirResourceLocatorMapFactory.class);

	private final Class<?> loadingClass;

	private final Supplier<? extends T> supplier;

	public ClasspathDirResourceLocatorMapFactory(final Class<?> loadingClass, final Supplier<? extends T> supplier) {
		this.loadingClass = loadingClass;
		this.supplier = supplier;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public T apply(final String dirToMap) {
		final T result = supplier.get();

		try (BufferedReader br = new BufferedReader(
				new InputStreamReader(loadingClass.getResourceAsStream(dirToMap)))) {
			for (String line = br.readLine(); line != null; line = br.readLine()) {
				final String[] filenameParts = FILE_EXT_SPLITTING_PATTERN.split(line);
				final String filenameBase = filenameParts[0];
				assert !result.containsKey(filenameBase);
				final URL resource = loadingClass.getResource(dirToMap + "/" + line);
				result.put(filenameBase, resource);
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

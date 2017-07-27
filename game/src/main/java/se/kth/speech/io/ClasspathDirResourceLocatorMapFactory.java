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
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
public final class ClasspathDirResourceLocatorMapFactory<K, M extends Map<K, URL>> implements Function<String, M> {

	private static class ClassLoadingResDirStreamFactory implements Function<String, Stream<String>> {

		private final Class<?> loadingClass;

		private ClassLoadingResDirStreamFactory(final Class<?> loadingClass) {
			this.loadingClass = loadingClass;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		public Stream<String> apply(final String dirName) {
			final InputStream is = loadingClass.getResourceAsStream(dirName);
			final BufferedReader br = new BufferedReader(new InputStreamReader(is));
			return br.lines();
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(ClasspathDirResourceLocatorMapFactory.class);

	private final Function<? super String, ? extends K> fileResourceNameFactory;

	private final Predicate<? super String> pathFilter;

	private final Function<? super String, ? extends URL> resourceUrlFactory;

	private final Supplier<? extends M> supplier;

	private final Function<? super String, Stream<String>> resDirStreamFactory;

	public ClasspathDirResourceLocatorMapFactory(final Class<?> loadingClass, final Supplier<? extends M> supplier,
			final Predicate<? super String> pathFilter,
			final Function<? super String, ? extends K> fileResourceNameFactory) {
		this(new ClassLoadingResDirStreamFactory(loadingClass), loadingClass::getResource, supplier, pathFilter,
				fileResourceNameFactory);
	}

	public ClasspathDirResourceLocatorMapFactory(final Function<? super String, Stream<String>> resDirStreamFactory,
			final Function<? super String, ? extends URL> resourceUrlFactory, final Supplier<? extends M> supplier,
			final Predicate<? super String> pathFilter,
			final Function<? super String, ? extends K> fileResourceNameFactory) {
		this.resDirStreamFactory = resDirStreamFactory;
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
		final M result = supplier.get();

		try (final Stream<String> dirInstream = resDirStreamFactory.apply(dirToMap)) {
			final Stream<String> validPaths = dirInstream.filter(pathFilter);
			validPaths.forEach(validPath -> {
				final K fileResourceName = fileResourceNameFactory.apply(validPath);
				final URL resource = resourceUrlFactory.apply(dirToMap + "/" + validPath);
				LOGGER.debug("Created URL \"{}\" for resource name \"{}\".", resource, fileResourceName);
				result.put(fileResourceName, resource);
			});
		}

		return result;
	}

}

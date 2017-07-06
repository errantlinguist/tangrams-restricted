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
package se.kth.speech.coin.tangrams.content;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.NavigableMap;
import java.util.Properties;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.Lists;
import se.kth.speech.io.ClasspathDirResourceLocatorMapFactory;
import se.kth.speech.io.FileNames;
import se.kth.speech.io.FileResourceLocatorContentTypePatternFilter;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
public final class IconImages {

	private static final Comparator<String> ICON_NAME_COMPARATOR;

	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");

	private static final Function<String, String> RESOURCE_NAME_FACTORY;

	static {
		try {
			final Properties props = ClassProperties.load(IconImages.class);
			final String imageOrderingStr = props.getProperty("image.ordering");
			final List<String> imageOrderingNames = Arrays
					.asList(MULTIVALUE_PROP_DELIM_PATTERN.split(imageOrderingStr));
			ICON_NAME_COMPARATOR = Comparator
					.nullsLast(Lists.comparingByIndex(imageOrderingNames).thenComparing(Comparator.naturalOrder()));

			RESOURCE_NAME_FACTORY = resourceLoc -> FileNames.splitBase(resourceLoc)[0];
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	public static NavigableMap<String, URL> createImageResourceMap() {
		return createImageResourceMap("image/(?!svg).+", RESOURCE_NAME_FACTORY);
	}

	/**
	 * @return the named icon image resources
	 */
	public static NavigableMap<String, URL> createImageResourceMap(final String resourceContentTypeRegex) {
		return createImageResourceMap(resourceContentTypeRegex, RESOURCE_NAME_FACTORY);
	}

	/**
	 * @return the resourceNameFactory
	 */
	public static Function<String, String> getResourceNameFactory() {
		return RESOURCE_NAME_FACTORY;
	}

	private static NavigableMap<String, URL> createImageResourceMap(final String resourceContentTypeRegex,
			final Function<? super String, String> resourceNameFactory) {
		final Predicate<String> imgFilter = new FileResourceLocatorContentTypePatternFilter(
				Pattern.compile(resourceContentTypeRegex));
		return new ClasspathDirResourceLocatorMapFactory<>(IconImages.class, () -> new TreeMap<>(ICON_NAME_COMPARATOR),
				imgFilter, resourceNameFactory).apply(ImageType.ICON.getDirLocator());
	}

	private IconImages() {

	}

}

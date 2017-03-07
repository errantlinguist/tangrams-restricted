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
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Properties;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.regex.Pattern;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.FilenameBaseSplitter;
import se.kth.speech.io.ClasspathDirResourceLocatorMapFactory;
import se.kth.speech.io.FileResourceLocatorContentTypePatternFilter;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
final class IconImages {

	private static final NavigableMap<String, URL> ICON_IMAGE_RESOURCES;

	private static final Comparator<String> ICON_NAME_COMPARATOR;

	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");

	static {
		try {
			final Properties props = ClassProperties.load(IconImages.class);
			final String imageOrderingStr = props.getProperty("image.ordering");
			final String[] imageOrderingNames = MULTIVALUE_PROP_DELIM_PATTERN.split(imageOrderingStr);
			final Map<String, Integer> imageOrderingIndices = new HashMap<>(imageOrderingNames.length + 1);
			int idx = 0;
			for (final String imageOrderingName : imageOrderingNames) {
				imageOrderingIndices.put(imageOrderingName, idx++);
			}
			ICON_NAME_COMPARATOR = new Comparator<String>() {

				private final Comparator<Integer> idxComparator = Comparator.nullsLast(Comparator.naturalOrder());

				/*
				 * (non-Javadoc)
				 *
				 * @see java.util.Comparator#compare(java.lang.Object,
				 * java.lang.Object)
				 */
				@Override
				public int compare(final String o1, final String o2) {
					final Integer idx1 = imageOrderingIndices.get(o1);
					final Integer idx2 = imageOrderingIndices.get(o2);
					int result = idxComparator.compare(idx1, idx2);
					if (result == 0) {
						// Fall back to alphanumeric ordering
						result = o1.compareTo(o2);
					}
					return result;
				}

			};

			final Predicate<String> imgFilter = new FileResourceLocatorContentTypePatternFilter(
					Pattern.compile("image/svg.+"));
			ICON_IMAGE_RESOURCES = new ClasspathDirResourceLocatorMapFactory<>(IconImages.class,
					() -> new TreeMap<>(ICON_NAME_COMPARATOR), imgFilter, new FilenameBaseSplitter())
							.apply(ImageType.ICON.getDirLocator());
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	/**
	 * @return the sortedImgResources
	 */
	static NavigableMap<String, URL> getIconImageResources() {
		return ICON_IMAGE_RESOURCES;
	}

	private IconImages() {

	}

}

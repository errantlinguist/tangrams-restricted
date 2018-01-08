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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.NavigableMap;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.Lists;
import se.kth.speech.io.FileNames;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
public final class IconImages {

	private static final Comparator<String> ICON_NAME_COMPARATOR;

	private static final Set<String> IMAGE_RESOURCE_NAMES;

	private static final String IMG_FILENAME_SUFFIX;
	
	private static final Logger LOGGER = LoggerFactory.getLogger(IconImages.class);
	
	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");

	private static final Function<String, String> RESOURCE_NAME_FACTORY = resourceLoc -> FileNames
			.splitBase(resourceLoc)[0];
	
	static {
		try {
			final Properties props = ClassProperties.load(IconImages.class);
			IMG_FILENAME_SUFFIX = props.getProperty("image.filenameSuffix");
			final String imageOrderingStr = props.getProperty("image.ordering");
			List<String> orderedImgNames = Arrays
					.asList(MULTIVALUE_PROP_DELIM_PATTERN.split(imageOrderingStr));
			ICON_NAME_COMPARATOR = Comparator
					.nullsLast(Lists.comparingByIndex(orderedImgNames).thenComparing(Comparator.naturalOrder()));
			IMAGE_RESOURCE_NAMES = Collections.unmodifiableSet(new HashSet<>(orderedImgNames));
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}
	
	public static NavigableMap<String, URL> createImageResourceMap(final Function<? super String, ? extends URL> resourceUrlFactory) {
		NavigableMap<String, URL> result = new TreeMap<>(ICON_NAME_COMPARATOR);
		for (String imgName : IMAGE_RESOURCE_NAMES){
			String resLoc = "/se/kth/speech/coin/tangrams/content/" + ImageType.ICON.getDirLocator() + '/' + imgName + IMG_FILENAME_SUFFIX;
			LOGGER.debug("Processing image file at \"{}\".", resLoc);
			URL imgUrl = resourceUrlFactory.apply(resLoc);
			LOGGER.debug("URL for image \"{}\" is \"{}\".", imgName, imgUrl);
			result.put(imgName, imgUrl);
		}
		return result;
	}
	
	/**
	 * @return the resourceNameFactory
	 */
	public static Function<String, String> getResourceNameFactory() {
		return RESOURCE_NAME_FACTORY;
	}

	static Set<String> getImageResourceNames(){
		return IMAGE_RESOURCE_NAMES;
	}
	
	static URL getImageUrl(ImageType imgType, String imgName){
		return imgType.getFileUrl(imgName + IMG_FILENAME_SUFFIX);
	}

	private IconImages() {

	}

}

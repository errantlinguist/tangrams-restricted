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
package se.kth.speech.coin.tangrams.analysis.features;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.Properties;
import java.util.function.ToDoubleFunction;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 *
 */
public final class ImageEdgeCounter implements ToDoubleFunction<String> {

	// private static final Logger LOGGER =
	// LoggerFactory.getLogger(ImageEdgeCountFactory.class);

	private static Object2DoubleMap<String> createPropValMap(final Properties props) {
		final Object2DoubleMap<String> result = new Object2DoubleOpenHashMap<>(props.size());
		result.defaultReturnValue(-1.0);
		props.forEach((propName, propValue) -> {
			result.put(propName.toString(), Double.parseDouble(propValue.toString()));
		});
		return result;
	}

	private static Properties loadEdgeCountProps() {
		final Properties result = new Properties();
		try (InputStream instream = ImageEdgeCounter.class.getResourceAsStream("edgeCounts.properties")) {
			result.load(instream);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
		return result;
	}

	private final Object2DoubleMap<String> resourceEdgeCounts;

	public ImageEdgeCounter() {
		this(createPropValMap(loadEdgeCountProps()));
	}

	public ImageEdgeCounter(final Object2DoubleMap<String> resourceEdgeCounts) {
		this.resourceEdgeCounts = resourceEdgeCounts;
	}

	// private static final Function<String, URL> LOCAL_RES_LOC_FACTORY =
	// IconImages.getImageResources()::get;

	@Override
	public double applyAsDouble(final String resourceName) {
		// URL resLoc = LOCAL_RES_LOC_FACTORY.apply(resourceName);
		// LOGGER.info("Reading file at \"{}\".", resLoc);
		// try {
		// MBFImage image = ImageUtilities.readMBF(resLoc);
		// image.processInplace(EDGE_DETECTOR);
		// } catch (IOException e) {
		// throw new UncheckedIOException(e);
		// }
		// TODO: Algorithmically count the edges of any arbitrary shape
		assert resourceEdgeCounts.containsKey(resourceName);
		return resourceEdgeCounts.getDouble(resourceName);
	}

}

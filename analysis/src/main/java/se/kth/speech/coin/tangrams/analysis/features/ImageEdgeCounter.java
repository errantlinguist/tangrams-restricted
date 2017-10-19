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
import java.util.function.ToIntFunction;

import javax.inject.Named;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 *
 */
@Named
public final class ImageEdgeCounter implements ToIntFunction<String> {

	private static Object2IntMap<String> createPropValMap(final Properties props) {
		final Object2IntMap<String> result = new Object2IntOpenHashMap<>(props.size());
		result.defaultReturnValue(-1);
		props.forEach((propName, propValue) -> {
			result.put(propName.toString(), Integer.parseInt(propValue.toString()));
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

	private final Object2IntMap<String> resourceEdgeCounts;

	public ImageEdgeCounter() {
		this(createPropValMap(loadEdgeCountProps()));
	}

	public ImageEdgeCounter(final Object2IntMap<String> resourceEdgeCounts) { // NO_UCD
																				// (use
																				// private)
		this.resourceEdgeCounts = resourceEdgeCounts;
	}

	// private static final Function<String, URL> LOCAL_RES_LOC_FACTORY =
	// IconImages.createImageResourceMap()::get;

	@Override
	public int applyAsInt(final String resourceName) {
		// URL resLoc = LOCAL_RES_LOC_FACTORY.apply(resourceName);
		// LOGGER.info("Reading file at \"{}\".", resLoc);
		// try {
		// MBFImage image = ImageUtilities.readMBF(resLoc);
		// image.processInplace(EDGE_DETECTOR);
		// } catch (IOException e) {
		// throw new UncheckedIOException(e);
		// }
		// TODO: Algorithmically count the edges of any arbitrary shape
		final int result = resourceEdgeCounts.getInt(resourceName);
		assert result != resourceEdgeCounts.defaultReturnValue();
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof ImageEdgeCounter)) {
			return false;
		}
		final ImageEdgeCounter other = (ImageEdgeCounter) obj;
		if (resourceEdgeCounts == null) {
			if (other.resourceEdgeCounts != null) {
				return false;
			}
		} else if (!resourceEdgeCounts.equals(other.resourceEdgeCounts)) {
			return false;
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (resourceEdgeCounts == null ? 0 : resourceEdgeCounts.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder((resourceEdgeCounts.size() + 1) * 16);
		builder.append("ImageEdgeCounter [resourceEdgeCounts=");
		builder.append(resourceEdgeCounts);
		builder.append("]");
		return builder.toString();
	}

}

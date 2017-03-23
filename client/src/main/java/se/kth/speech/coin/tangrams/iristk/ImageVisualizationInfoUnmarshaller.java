/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.iristk;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

final class ImageVisualizationInfoUnmarshaller
		implements Function<ImageVisualizationInfoDescription, ImageVisualizationInfo> {

	private final Function<? super String, ? extends URL> resourceNameLocFactory;

	ImageVisualizationInfoUnmarshaller(final Function<? super String, ? extends URL> resourceNameLocFactory) {
		this.resourceNameLocFactory = resourceNameLocFactory;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public ImageVisualizationInfo apply(final ImageVisualizationInfoDescription desc) {
		final List<ImageVisualizationInfoDescription.Datum> descData = desc.getData();
		final List<ImageVisualizationInfo.Datum> data = descData.stream().map(this::createImageVisualizationInfo)
				.collect(Collectors.toCollection(() -> new ArrayList<>(descData.size())));
		return new ImageVisualizationInfo(data, desc.getUniqueImgResourceCount(), new HashSet<>(desc.getColors()));
	}

	private ImageVisualizationInfo.Datum createImageVisualizationInfo(
			final ImageVisualizationInfoDescription.Datum desc) {
		final String resourceName = desc.getResourceName();
		final URL resourceLoc = resourceNameLocFactory.apply(resourceName);
		return new ImageVisualizationInfo.Datum(resourceLoc, desc.getColor(), desc.getSize());
	}

}
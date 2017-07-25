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
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;

public final class ImageVisualizationInfoUnmarshaller
		implements Function<ImageVisualizationInfoDescription, ImageVisualizationInfo> {

	private final Function<? super String, ? extends URL> resourceNameLocFactory;

	public ImageVisualizationInfoUnmarshaller() {
		this(IconImages.createImageResourceMap()::get);
	}

	public ImageVisualizationInfoUnmarshaller(final Function<? super String, ? extends URL> resourceNameLocFactory) {
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
		final List<ImageVisualizationInfo.Datum> data = Arrays.asList(
				descData.stream().map(this::createImageVisualizationInfo).toArray(ImageVisualizationInfo.Datum[]::new));
		return new ImageVisualizationInfo(data, desc.getUniqueImgResourceCount());
	}

	private ImageVisualizationInfo.Datum createImageVisualizationInfo(
			final ImageVisualizationInfoDescription.Datum desc) {
		final String resourceName = desc.getResourceName();
		final URL resourceLoc = resourceNameLocFactory.apply(resourceName);
		return new ImageVisualizationInfo.Datum(resourceLoc, desc.getColor(), desc.getSize());
	}

}
/*
 *  This file is part of game.
 *
 *  game is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.iristk.events;

import java.awt.Color;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

import iristk.util.Record;
import se.kth.speech.URLFilenameFactory;
import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.coin.tangrams.content.ImageSize;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Mar 2017
 *
 */
public final class ImageVisualizationInfoDescription extends Record {

	public static final class Datum extends Record {

		@RecordField(name = "color")
		private int color;

		private String resourceName;

		@RecordField(name = "size")
		private String size;

		public Datum() {
			// Default constructor is required for JSON (un-)marshalling
		}

		public Datum(final ImageVisualizationInfo.Datum imgVisualizationInfo) {
			setResourceName(imgVisualizationInfo.getResourceLoc());
			setColor(imgVisualizationInfo.getColor());
			setSize(imgVisualizationInfo.getSize());
		}

		/**
		 * @return the color
		 */
		public Color getColor() {
			return new Color(color, true);
		}

		/**
		 * @return the resourceName
		 */
		@RecordField(name = "resourceName")
		public String getResourceName() {
			return resourceName;
		}

		/**
		 * @return the size
		 */
		public ImageSize getSize() {
			return ImageSize.valueOf(size);
		}

		public void setColor(final Color color) {
			this.color = color.getRGB();
		}

		@RecordField(name = "resourceName")
		public void setResourceName(final String resourceName) {
			this.resourceName = resourceName;
		}

		public void setResourceName(final URL resourceLoc) {
			setResourceName(RESOURCE_NAME_FACTORY.apply(resourceLoc));
		}

		public void setSize(final ImageSize size) {
			this.size = size.toString();
		}

	}

	private static final Function<URL, String> RESOURCE_NAME_FACTORY = new URLFilenameFactory()
			.andThen(IconImages.getResourceNameFactory());

	/**
	 * @return the resourceNameFactory
	 */
	public static Function<URL, String> getResourceNameFactory() {
		return RESOURCE_NAME_FACTORY;
	}

	private List<Datum> data;

	private int uniqueImgResourceCount;

	public ImageVisualizationInfoDescription() {
		// Default constructor is required for JSON (un-)marshalling
	}

	public ImageVisualizationInfoDescription(final ImageVisualizationInfo imgVizInfo) {
		final List<ImageVisualizationInfo.Datum> imgVizInfoData = imgVizInfo.getData();
		final List<Datum> data = Arrays.asList(imgVizInfoData.stream().map(Datum::new).toArray(Datum[]::new));
		setData(data);
		setUniqueImgResourceCount(imgVizInfo.getUniqueImageResourceCount());
	}

	public ImageVisualizationInfoDescription(final List<Datum> data, final int uniqueImgResourceCount) {
		setData(data);
		setUniqueImgResourceCount(uniqueImgResourceCount);
	}

	/**
	 * @return the data
	 */
	@RecordField(name = "data")
	public List<Datum> getData() {
		return data;
	}

	/**
	 * @return the uniqueImgResourceCount
	 */
	@RecordField(name = "uniqueImgResourceCount")
	public int getUniqueImgResourceCount() {
		return uniqueImgResourceCount;
	}

	/**
	 * @param data
	 *            the data to set
	 */
	@RecordField(name = "data")
	public void setData(final List<Datum> data) {
		this.data = data;
	}

	/**
	 * @param uniqueImgResourceCount
	 *            the uniqueImgResourceCount to set
	 */
	@RecordField(name = "uniqueImgResourceCount")
	public void setUniqueImgResourceCount(final int uniqueImgResourceCount) {
		this.uniqueImgResourceCount = uniqueImgResourceCount;
	}

}

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
package se.kth.speech.coin.tangrams.content;

import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class RandomPieceImageManager {

	private final Function<Random, Stream<ImageVisualizationInfo>> rndImgDataFactory = new RandomPieceImageDataFactory(
			IconImages.getIconImageResources().entrySet(), 2);

	/**
	 * @param pieceCount
	 *            The number of pieces to create image data for.
	 */
	public RandomPieceImageManager(final int pieceCount) {
		// TODO Ensure even distribution of shapes, sizes and colors
	}

	public List<ImageVisualizationInfo> createImageData(final Random rnd) {
		return rndImgDataFactory.apply(rnd).collect(Collectors.toList());
	}

	// private Table<ImageSize, Color, Set<URL>> createColorSizeImageMap(final
	// List<ImageVisualizationInfo> imgData) {
	// final Table<ImageSize, Color, Set<URL>> result =
	// HashBasedTable.create(sizes.size(), uniqueImgColors.size());
	// for (final ImageVisualizationInfo imgDatum : imgData) {
	// final URL resourceLoc = imgDatum.getResourceLoc();
	// final ImageSize size = imgDatum.getSize();
	// final Map<Color, Set<URL>> oldSizeColors = result.row(size);
	// if (oldSizeColors == null) {
	// // The given size has never been used before
	// final Set<URL> resourceLocs =
	// Sets.newHashSetWithExpectedSize(Math.min(imgResources.size(), 16));
	// resourceLocs.add(resourceLoc);
	// result.put(size, imgDatum.getColor(), resourceLocs);
	// } else {
	// final Color color = imgDatum.getColor();
	// final Set<URL> oldResourceLocs = oldSizeColors.get(color);
	// if (oldResourceLocs == null) {
	// // The given combination of color and size has never been
	// // used for this resource before
	// final Set<URL> newResourceLocs =
	// Sets.newHashSetWithExpectedSize(Math.min(imgResources.size(), 16));
	// newResourceLocs.add(resourceLoc);
	// oldSizeColors.put(color, newResourceLocs);
	// } else if (oldResourceLocs.add(resourceLoc)) {
	// LOGGER.debug("Resource \"{}\" added for size {} and color \"{}\".",
	// new Object[] { resourceLoc, size, color, size });
	// } else {
	// LOGGER.warn("Resource \"{}\" already added for size {} and color
	// \"{}\".",
	// new Object[] { resourceLoc, size, color, size });
	// }
	// }
	// }
	// return result;
	// }
	//
	// private Map<URL, Table<ImageSize, Color, ImageVisualizationInfo>>
	// createImageDataAttrTableMap(final List<ImageVisualizationInfo> imgData) {
	// final Map<URL, Table<ImageSize, Color, ImageVisualizationInfo>> result =
	// Maps.newHashMapWithExpectedSize(imgData.size());
	//
	// for (final ImageVisualizationInfo imgDatum : imgData) {
	// final URL resourceLoc = imgDatum.getResourceLoc();
	// int commonAttrCount = 0;
	// final Table<ImageSize, Color, ImageVisualizationInfo> oldResourceImgDataTable =
	// result.get(resourceLoc);
	// if (oldResourceImgDataTable == null) {
	// // The given resource locator has never been used
	// // before
	// final Table<ImageSize, Color, ImageVisualizationInfo> newResourceImgDataTable =
	// HashBasedTable.create(sizes.size(),
	// uniqueImgColors.size());
	// newResourceImgDataTable.put(imgDatum.getSize(), imgDatum.getColor(),
	// imgDatum);
	// result.put(resourceLoc, newResourceImgDataTable);
	// } else {
	// // Check size
	// final ImageSize size = imgDatum.getSize();
	// final Map<Color, ImageVisualizationInfo> imgDataByColor =
	// oldResourceImgDataTable.row(size);
	// if (imgDataByColor == null) {
	// // The given size has never been used for
	// // this resource before
	// LOGGER.debug("Observed {} for resource \"{}\" for the first time.", size,
	// resourceLoc);
	// // Try looking up the image data by color instead
	// final Color color = imgDatum.getColor();
	// final Map<ImageSize, ImageVisualizationInfo> imgDataBySize =
	// oldResourceImgDataTable.column(color);
	// if (imgDataBySize == null) {
	// LOGGER.debug("Observed {} for resource \"{}\" for the first time.",
	// color, resourceLoc);
	// oldResourceImgDataTable.put(size, color, imgDatum);
	// } else {
	// // The given resource has been seen for the given color
	// // but not the given size
	// commonAttrCount++;
	// if (maxSharedAttrCount < commonAttrCount) {
	// LOGGER.debug("Maximum number of common attributes reached for resource
	// \"{}\"; Skipping.",
	// resourceLoc);
	// break;
	// } else if (!Objects.equals(imgDataBySize.put(size, imgDatum), imgDatum))
	// {
	// // Sanity check
	// throw new IllegalArgumentException("Image data duplicate.");
	// }
	// }
	//
	// } else {
	// // The given size has already been used at least once for
	// // this resource
	// commonAttrCount++;
	// if (maxSharedAttrCount < commonAttrCount) {
	// LOGGER.debug("Maximum number of common attributes reached for resource
	// \"{}\"; Skipping.",
	// resourceLoc);
	// break;
	// } else if (!Objects.equals(imgDataByColor.put(imgDatum.getColor(),
	// imgDatum), imgDatum)) {
	// // Sanity check
	// throw new IllegalArgumentException("Image data duplicate.");
	// }
	// }
	// }
	// }
	//
	// return result;
	// }

}

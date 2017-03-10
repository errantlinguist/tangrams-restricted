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
package se.kth.speech.coin.tangrams.view;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMap.Region;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
final class RandomImageMatrixSpatialRegionFactory implements BiFunction<ImageViewInfo, Random, SpatialMap.Region> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RandomImageMatrixSpatialRegionFactory.class);

	private static int[] createPosMatrixBoundsArray(final ImageViewInfo viewInfo) {
		final ImageViewInfo.RasterizationInfo rasterizationInfo = viewInfo.getRasterization();
		// NOTE: "rows" in the matrix go top-bottom and "cols" go left-right
		// The number of rows this image takes up in the
		// position matrix
		final int occupiedPosMatrixRowCount = rasterizationInfo.getHeight() / rasterizationInfo.getGcd();
		// The number of columns this image takes up in the
		// position matrix
		final int occupiedPosMatrixColCount = rasterizationInfo.getWidth() / rasterizationInfo.getGcd();
		LOGGER.debug("Calculated position grid size {}*{} for \"{}\".", new Object[] {
				occupiedPosMatrixRowCount, occupiedPosMatrixColCount, viewInfo.getVisualization().getResourceLoc() });

		return new int[] { occupiedPosMatrixRowCount, occupiedPosMatrixColCount };
	}

	private static SpatialMap.Region createSpatialRegion(final int[] startMatrixIdx, final int[] endMatrixIdx) {
		return new SpatialMap.Region(startMatrixIdx[0], endMatrixIdx[0], startMatrixIdx[1], endMatrixIdx[1]);
	}

	private final Map<ImageViewInfo, int[]> piecePosMatrixSizes;
	
	private final int[] posDims;

	RandomImageMatrixSpatialRegionFactory(final int[] posDims) {
		this.posDims = posDims;
		piecePosMatrixSizes = new HashMap<>();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.function.BiFunction#apply(java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public Region apply(final ImageViewInfo viewInfo, final Random rnd) {
		// The number of rows and columns this image takes up in the
		// position matrix
		final int[] piecePosMatrixSize = piecePosMatrixSizes.computeIfAbsent(viewInfo, RandomImageMatrixSpatialRegionFactory::createPosMatrixBoundsArray);
		// Randomly pick a space in the matrix
		return createRandomSpatialRegion(piecePosMatrixSize, rnd);
	}

	private SpatialMap.Region createRandomSpatialRegion(final int[] piecePosMatrixSize, final Random rnd) {
		final IntStream maxPossibleMatrixIdxs = IntStream.range(0, posDims.length)
				.map(i -> posDims[i] - piecePosMatrixSize[i] + 1);
		// Randomly pick a space in the matrix
		final int[] startMatrixIdx = maxPossibleMatrixIdxs.map(rnd::nextInt).toArray();
		final int[] endMatrixIdx = IntStream.range(0, startMatrixIdx.length)
				.map(i -> startMatrixIdx[i] + piecePosMatrixSize[i]).toArray();
		return createSpatialRegion(startMatrixIdx, endMatrixIdx);
	}

}

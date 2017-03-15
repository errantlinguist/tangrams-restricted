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

import java.util.Map;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Mar 2017
 *
 */
final class CachingPieceMatrixBoundsArrayFactory implements Function<ImageViewInfo, int[]> {

	private static final Logger LOGGER = LoggerFactory.getLogger(CachingPieceMatrixBoundsArrayFactory.class);

	private final Map<ImageViewInfo, int[]> instances;

	CachingPieceMatrixBoundsArrayFactory(final Map<ImageViewInfo, int[]> instances) {
		this.instances = instances;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public int[] apply(final ImageViewInfo viewInfo) {
		// The number of rows and columns this image takes up in the
		// position matrix
		return instances.computeIfAbsent(viewInfo, this::createPosMatrixBoundsArray);
	}

	private int[] createPosMatrixBoundsArray(final ImageViewInfo viewInfo) {
		final ImageViewInfo.RasterizationInfo rasterizationInfo = viewInfo.getRasterization();
		// NOTE: "rows" in the matrix go top-bottom and "cols" go left-right
		// The number of rows this image takes up in the
		// position matrix
		final int occupiedPosMatrixRowCount = rasterizationInfo.getHeight() / rasterizationInfo.getGcd();
		// The number of columns this image takes up in the
		// position matrix
		final int occupiedPosMatrixColCount = rasterizationInfo.getWidth() / rasterizationInfo.getGcd();
		LOGGER.debug("Calculated position grid size {}*{} for \"{}\".", new Object[] { occupiedPosMatrixRowCount,
				occupiedPosMatrixColCount, viewInfo.getVisualization().getResourceLoc() });

		return new int[] { occupiedPosMatrixRowCount, occupiedPosMatrixColCount };
	}

}

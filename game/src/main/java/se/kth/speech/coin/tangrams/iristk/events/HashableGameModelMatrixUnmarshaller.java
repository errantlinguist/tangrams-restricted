/*
 *  This file is part of client.
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
package se.kth.speech.coin.tangrams.iristk.events;

import java.util.List;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Matrix;
import se.kth.speech.SpatialMatrix;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 19 Oct 2017
 *
 */
public final class HashableGameModelMatrixUnmarshaller
		implements Function<HashableModelDescription, SpatialMatrix<Integer>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(HashableGameModelMatrixUnmarshaller.class);

	private final SpatialMatrix.Factory matrixFactory;

	public HashableGameModelMatrixUnmarshaller(final SpatialMatrix.Factory matrixFactory) {
		this.matrixFactory = matrixFactory;
	}

	@Override
	public SpatialMatrix<Integer> apply(final HashableModelDescription modelDesc) {
		final List<Integer> coordOccupants = modelDesc.getCoordOccupants();
		LOGGER.debug("Creating model with coord occupant vector: {}", coordOccupants);
		final int colCount = modelDesc.getColCount();
		final Matrix<Integer> backingMatrix = new Matrix<>(coordOccupants, colCount);
		return matrixFactory.create(backingMatrix);
	}

}

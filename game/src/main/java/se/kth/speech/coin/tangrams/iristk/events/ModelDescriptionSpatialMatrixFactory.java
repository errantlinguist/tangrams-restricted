/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Integers;
import se.kth.speech.Matrix;
import se.kth.speech.SpatialMatrix;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 25 Jan 2017
 *
 */
public final class ModelDescriptionSpatialMatrixFactory implements Function<ModelDescription, SpatialMatrix<Integer>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ModelDescriptionSpatialMatrixFactory.class);

	private static final Function<String, Integer> NULLABLE_INTEGER_GETTER = Integers::valueOfNullable;

	private final SpatialMatrix.Factory spatialMatrixFactory;

	public ModelDescriptionSpatialMatrixFactory(final SpatialMatrix.Factory spatialMatrixFactory) {
		this.spatialMatrixFactory = spatialMatrixFactory;
	}

	@Override
	public SpatialMatrix<Integer> apply(final ModelDescription modelDesc) {
		final List<String> nullableCoordOccupants = modelDesc.getCoordOccupants();
		final List<Integer> coordOccupants = Arrays
				.asList(nullableCoordOccupants.stream().map(NULLABLE_INTEGER_GETTER).toArray(Integer[]::new));
		LOGGER.debug("Creating model with coord occupant vector: {}", coordOccupants);
		final int colCount = modelDesc.getColCount();
		final Matrix<Integer> backingMatrix = new Matrix<>(coordOccupants, colCount);
		return spatialMatrixFactory.create(backingMatrix);
	}

}

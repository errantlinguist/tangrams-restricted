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
package se.kth.speech.coin.tangrams.iristk;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Integers;
import se.kth.speech.Matrix;
import se.kth.speech.SpatialMatrix;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 25 Jan 2017
 *
 */
final class GameStateUnmarshalling {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameStateUnmarshalling.class);

	private static final Function<String, Integer> NULLABLE_INTEGER_GETTER = Integers::valueOfNullable;

	public static SpatialMatrix<Integer> createModel(final ModelDescription modelDesc) {
		return createModel(modelDesc, SpatialMatrix.Factory.STABLE_ITER_ORDER);
	}

	public static SpatialMatrix<Integer> createModel(final ModelDescription modelDesc,
			final SpatialMatrix.Factory factory) {
		final List<String> nullableCoordOccupants = modelDesc.getCoordOccupants();
		final List<Integer> coordOccupants = nullableCoordOccupants.stream().map(NULLABLE_INTEGER_GETTER)
				.collect(Collectors.toCollection(() -> new ArrayList<>(nullableCoordOccupants.size())));
		LOGGER.debug("Creating model with coord occupant vector: {}", coordOccupants);
		final int colCount = modelDesc.getColCount();
		final Matrix<Integer> backingMatrix = new Matrix<>(coordOccupants, colCount);
		return factory.create(backingMatrix);
	}

	private GameStateUnmarshalling() {
	}

}

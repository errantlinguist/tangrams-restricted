/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.IntArrays;
import se.kth.speech.Matrix;
import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.game.Model;
import se.kth.speech.coin.tangrams.game.RemoteController;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Jan 2017
 *
 */
final class GameFactory implements Function<String, Game<Integer>> {

	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");;

	private static final Properties PROPS;

	private static final String TEST_GAME_NAME = "test";

	static {
		try {
			PROPS = ClassProperties.load(GameFactory.class);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private static MutablePair<RandomModelFactory<Integer>, int[]> createModelFactory() {
		final int[] modelDims = MULTIVALUE_PROP_DELIM_PATTERN.splitAsStream(PROPS.getProperty("model.dims"))
				.mapToInt(Integer::parseInt).toArray();
		final int emptyCells = Integer.parseInt(PROPS.getProperty("model.emptyCells"));
		final int cellCount = IntArrays.product(modelDims);
		final int occupiedCellCount = cellCount - emptyCells;

		// Create a piece ID array just big enough to fill all the occupied
		// cells rather than every cell in the matrix
		final Integer[] coordOccupants = IntStream.range(0, occupiedCellCount).boxed().toArray(Integer[]::new);
		return new MutablePair<>(new RandomModelFactory<>(modelDims, coordOccupants), modelDims);
	}

	private final int colCount;

	private final Function<? super Random, ? extends Model<Integer>> modelFactory;

	public GameFactory() {
		this(createModelFactory());
	}

	private GameFactory(final Map.Entry<RandomModelFactory<Integer>, int[]> modelFactoryDims) {
		this(modelFactoryDims.getKey(), modelFactoryDims.getValue()[1]);
	}

	GameFactory(final Function<? super Random, ? extends Model<Integer>> modelFactory, final int colCount) {
		this.modelFactory = modelFactory;
		this.colCount = colCount;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Game<Integer> apply(final String name) {
		final Model<Integer> model;
		final Model<Integer> winningModel;
		final long seed;
		if (TEST_GAME_NAME.equals(name)) {
			// FIXME: This will break if you change the model dimension
			// properties
			final Integer[] coordOccupants = new Integer[] { 0, 7, 12, 9, 6, 5, 10, 4, 3, 1, 2, null, 8, null, null,
					11 };
			model = new Model<>(new Matrix<>(coordOccupants, colCount));
			final Integer[] winningCoordOccupants = new Integer[] { 0, 7, 12, 9, 6, 5, 10, 4, 3, 1, 2, null, 8, null,
					11, null };
			winningModel = new Model<>(new Matrix<>(winningCoordOccupants, colCount));
			seed = name.hashCode();
		} else {
			try {
				seed = Long.parseLong(name);
				final Random rnd = new Random(seed);
				model = modelFactory.apply(rnd);
				winningModel = modelFactory.apply(rnd);

			} catch (final NumberFormatException e) {
				throw new IllegalArgumentException("Invalid game name.", e);
			}
		}
		if (model.equals(winningModel)) {
			throw new AssertionError(
					"Supposedly-randomized winning configuration is equal to the original configuration.");
		}
		return new Game<>(new RemoteController<>(model, handoff -> {
		}, playerId -> true), winningModel, seed);
	}

}

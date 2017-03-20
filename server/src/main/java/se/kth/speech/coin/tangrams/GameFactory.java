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

import java.awt.Image;
import java.awt.Toolkit;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Properties;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.regex.Pattern;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.game.RemoteController;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Jan 2017
 *
 */
final class GameFactory implements Function<String, Game<Integer>> {

	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");;

	private static final Properties PROPS;

	static {
		try {
			PROPS = ClassProperties.load(GameFactory.class);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private static final BiFunction<Image, Toolkit, Image> DEFAULT_POST_COLORING_IMG_TRANSFORMER = new BiFunction<Image, Toolkit, Image>() {

		@Override
		public Image apply(final Image img, final Toolkit toolkit) {
			// Do nothing
			return img;
		}

	};

	private static RandomPopulatedModelFactory createModelFactory() {
		final boolean allowFailedPlacements = Boolean.parseBoolean(PROPS.getProperty("allowFailedPlacements"));
		final int[] gridDims = MULTIVALUE_PROP_DELIM_PATTERN.splitAsStream(PROPS.getProperty("gridDims"))
				.mapToInt(Integer::parseInt).toArray();
		final double occupiedGridArea = Double.parseDouble(PROPS.getProperty("occupiedGridArea"));
		final int piecePlacementCount = Integer.parseInt(PROPS.getProperty("piecePlacementCount"));

		return new RandomPopulatedModelFactory(gridDims, Toolkit.getDefaultToolkit(), piecePlacementCount,
				occupiedGridArea, DEFAULT_POST_COLORING_IMG_TRANSFORMER, allowFailedPlacements);
	}

	private final Function<? super Random, ? extends SpatialMatrix<Integer>> modelFactory;

	public GameFactory() {
		this(createModelFactory());
	}

	GameFactory(final Function<? super Random, SpatialMatrix<Integer>> modelFactory) {
		this.modelFactory = modelFactory;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Game<Integer> apply(final String name) {
		final SpatialMatrix<Integer> model;
		final long seed;
		try {
			seed = Long.parseLong(name);
			final Random rnd = new Random(seed);
			model = modelFactory.apply(rnd);

		} catch (final NumberFormatException e) {
			throw new IllegalArgumentException("Invalid game name.", e);
		}
		return new Game<>(new RemoteController<>(model, handoff -> {
		}, playerId -> true), seed);
	}

}

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
package se.kth.speech.coin.tangrams.game;

import java.awt.Color;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.List;
import java.util.Properties;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.github.errantlinguist.ClassProperties;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.awt.RandomHueColorFactory;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.RandomImageVisualizationInfoFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Jan 2017
 *
 */
public final class GameFactory implements Function<String, Game<Integer>> {

	private static final BiFunction<Image, Toolkit, Image> DEFAULT_POST_COLORING_IMG_TRANSFORMER = new BiFunction<Image, Toolkit, Image>() {

		@Override
		public Image apply(final Image img, final Toolkit toolkit) {
			// Do nothing
			return img;
		}

	};

	private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");

	private static List<Color> createDefaultLengthRandomColorList(final Random rnd,
			final Collection<? super Color> blacklistedColors) {
		final RandomHueColorFactory colorFactory = new RandomHueColorFactory(rnd);
		final int size = 4;
		return Stream.generate(colorFactory).filter(color -> !blacklistedColors.contains(color)).distinct().limit(size)
				.collect(Collectors.toCollection(() -> new ArrayList<>(size)));
	}

	private static RandomPopulatedModelFactory createModelFactory(final ImageVisualizationInfo imgVizInfo,
			final Properties props) {
		final boolean allowFailedPlacements = Boolean.parseBoolean(props.getProperty("allowFailedPlacements"));
		final int[] gridDims = MULTIVALUE_PROP_DELIM_PATTERN.splitAsStream(props.getProperty("gridDims"))
				.mapToInt(Integer::parseInt).toArray();
		final double occupiedGridArea = Double.parseDouble(props.getProperty("occupiedGridArea"));
		return new RandomPopulatedModelFactory(gridDims, imgVizInfo, Toolkit.getDefaultToolkit(), occupiedGridArea,
				DEFAULT_POST_COLORING_IMG_TRANSFORMER, allowFailedPlacements);
	}

	private static Properties loadClassProps() {
		try {
			return ClassProperties.load(GameFactory.class);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private final Collection<? super Color> blacklistedColors;

	public GameFactory(final Collection<? super Color> blacklistedColors) {
		this.blacklistedColors = blacklistedColors;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Game<Integer> apply(final String name) {
		final Properties props = loadClassProps();

		final SpatialMatrix<Integer> imgModel;
		final ImageVisualizationInfo imgVisualizationInfo;
		final long seed;
		try {
			seed = Long.parseLong(name);
			final Random rnd = new Random(seed);
			final List<Color> colors = createDefaultLengthRandomColorList(rnd, blacklistedColors);
			final RandomImageVisualizationInfoFactory imgDataFactory = new RandomImageVisualizationInfoFactory(rnd,
					colors);
			final int pieceCount = Integer.parseInt(props.getProperty("pieceCount"));
			imgVisualizationInfo = imgDataFactory.apply(pieceCount);
			final RandomPopulatedModelFactory modelFactory = createModelFactory(imgVisualizationInfo, props);
			imgModel = modelFactory.apply(rnd);

		} catch (final NumberFormatException e) {
			throw new IllegalArgumentException("Invalid game name.", e);
		}
		return new Game<>(seed, imgModel, imgVisualizationInfo, new EnumMap<>(PlayerRole.class));
	}

}

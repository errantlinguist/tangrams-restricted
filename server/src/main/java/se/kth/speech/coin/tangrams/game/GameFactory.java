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
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.github.errantlinguist.ClassProperties;
import com.google.common.collect.Maps;

import se.kth.speech.RandomCollectionElementChooser;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.URLQueryParamMapFactory;
import se.kth.speech.awt.RandomHueColorFactory;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.RandomImageVisualizationInfoFactory;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 9 Jan 2017
 *
 */
public final class GameFactory implements Function<String, Game<Integer>> {

	public enum Parameter {
		ALLOW_FAILED_PLACEMENTS("allowFailedPlacements") {
			@Override
			protected Object parseValue(final String value) {
				return Boolean.valueOf(value);
			}
		},
		COLOR_COUNT("colorCount") {
			@Override
			protected Object parseValue(final String value) {
				return Integer.valueOf(value);
			}
		},
		GRID_DIMENSIONS("gridDims") {
			@Override
			protected Object parseValue(final String value) {
				return MULTIVALUE_PROP_DELIM_PATTERN.splitAsStream(value).mapToInt(Integer::parseInt).toArray();
			}
		},
		OCCUPIED_GRID_AREA("occupiedGridArea") {
			@Override
			protected Object parseValue(final String value) {
				return Double.valueOf(value);
			}
		},
		PIECE_COUNT("pieceCount") {
			@Override
			protected Object parseValue(final String value) {
				return Integer.valueOf(value);
			}
		},
		SEED("seed") {
			@Override
			protected Object parseValue(final String value) {
				return Long.valueOf(value);
			}
		};

		private static final Pattern MULTIVALUE_PROP_DELIM_PATTERN = Pattern.compile("\\s*,\\s*");

		final String paramName;

		private Parameter(final String paramName) {
			this.paramName = paramName;
		}

		protected Object parseValue(final Properties props) {
			final String val = props.getProperty(paramName);
			return parseValue(val);
		}

		protected abstract Object parseValue(String value);

	}

	private static class GameParamMapFactory implements Function<String, Map<Parameter, Object>> {

		private final Properties defaultProps;

		private GameParamMapFactory(final Properties defaultProps) {
			this.defaultProps = defaultProps;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		public Map<Parameter, Object> apply(final String name) {
			final Map<Parameter, Object> result = new EnumMap<>(Parameter.class);
			try {
				final Parameter singleParam = Parameter.SEED;
				final Object seed = singleParam.parseValue(name);
				result.put(singleParam, seed);
				for (final Parameter param : EnumSet.complementOf(EnumSet.of(Parameter.SEED))) {
					result.put(param, param.parseValue(defaultProps));
				}
			} catch (final NumberFormatException nfe) {
				final Map<Parameter, String> gameParams = GAME_PARAM_PARSER.apply(name);
				if (gameParams.isEmpty()) {
					throw new IllegalArgumentException("Invalid game name \"" + name + "\".", nfe);
				} else {
					for (final Parameter param : Parameter.values()) {
						String paramVal = gameParams.get(param);
						if (paramVal == null) {
							paramVal = defaultProps.getProperty(param.paramName);
						}
						final Object parsedVal = param.parseValue(paramVal);
						result.put(param, parsedVal);
					}
				}
			}
			assert result.size() == Parameter.values().length;
			return result;
		}

	}

	private static final BiFunction<Image, Toolkit, Image> DEFAULT_POST_COLORING_IMG_TRANSFORMER = new BiFunction<Image, Toolkit, Image>() {

		@Override
		public Image apply(final Image img, final Toolkit toolkit) {
			// Do nothing
			return img;
		}

	};

	private static final Function<String, Map<Parameter, String>> GAME_PARAM_PARSER;

	static {
		final Map<String, Parameter> namedParams = createNamedParamMap(Parameter.values(),
				Collections.singleton(Function.identity()));
		GAME_PARAM_PARSER = new URLQueryParamMapFactory(Function.identity()).andThen(params -> {
			final Map<Parameter, String> result = new EnumMap<>(Parameter.class);
			params.forEach((paramName, paramValue) -> {
				final Parameter param = namedParams.get(paramName);
				if (param == null) {
					throw new IllegalArgumentException("Could not parse parameter named \"" + paramName + "\".");
				}
				result.put(param, paramValue);
			});
			return result;
		});
	}

	private static Function<String, Map<Parameter, Object>> createGameParamMapFactory() {
		final Properties props = loadClassProps();
		return new GameParamMapFactory(props);
	}

	private static RandomPopulatedModelFactory createModelFactory(final ImageVisualizationInfo imgVizInfo,
			final Map<Parameter, Object> gameParams) {
		final Boolean allowFailedPlacements = (Boolean) gameParams.get(Parameter.ALLOW_FAILED_PLACEMENTS);
		final int[] gridDims = (int[]) gameParams.get(Parameter.GRID_DIMENSIONS);
		final Double occupiedGridArea = (Double) gameParams.get(Parameter.OCCUPIED_GRID_AREA);
		return new RandomPopulatedModelFactory(gridDims, imgVizInfo, Toolkit.getDefaultToolkit(), occupiedGridArea,
				DEFAULT_POST_COLORING_IMG_TRANSFORMER, allowFailedPlacements);
	}

	private static Map<String, Parameter> createNamedParamMap(final Parameter[] params,
			final Collection<? extends Function<? super String, String>> keyFactories) {
		final int resultSize = params.length * keyFactories.size();
		final Map<String, Parameter> result = Maps.newHashMapWithExpectedSize(resultSize);
		for (final Parameter param : params) {
			for (final Function<? super String, String> keyFactory : keyFactories) {
				final String key = keyFactory.apply(param.paramName);
				result.put(key, param);
			}
		}
		return result;
	}

	private static List<Color> createRandomColorList(final Random rnd,
			final Collection<? super Color> blacklistedColors, final int size) {
		final RandomHueColorFactory colorFactory = new RandomHueColorFactory(rnd);
		// NOTE: Using "Collectors.toCollection(...)" with an explicit size is
		// better here than using "toArray(..)" because Stream.filter(...) can't
		// deduce the result array size itself
		// <https://stackoverflow.com/questions/45275402/java-create-a-list-with-n-objects/45275859#comment-77531249>
		return Stream.generate(colorFactory).filter(color -> !blacklistedColors.contains(color)).distinct().limit(size)
				.collect(Collectors.toCollection(() -> new ArrayList<>(size)));
	}

	private static Properties loadClassProps() {
		try {
			return ClassProperties.load(GameFactory.class);
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private final Collection<? super Color> blacklistedColors;

	private final Function<String, Map<Parameter, Object>> gameParamMapFactory = createGameParamMapFactory();

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
		final Map<Parameter, Object> gameParams = gameParamMapFactory.apply(name);

		final Long seed = (Long) gameParams.get(Parameter.SEED);
		final Random rnd = new Random(seed);

		final Integer colorCount = (Integer) gameParams.get(Parameter.COLOR_COUNT);
		final List<Color> colors = createRandomColorList(rnd, blacklistedColors, colorCount);

		final RandomImageVisualizationInfoFactory imgDataFactory = new RandomImageVisualizationInfoFactory(rnd, colors);
		final Integer pieceCount = (Integer) gameParams.get(Parameter.PIECE_COUNT);
		final ImageVisualizationInfo imgVisualizationInfo = imgDataFactory.apply(pieceCount);

		final RandomPopulatedModelFactory modelFactory = createModelFactory(imgVisualizationInfo, gameParams);
		final SpatialMatrix<Integer> model = modelFactory.apply(new RandomCollectionElementChooser(rnd));

		return new Game<>(seed, model, imgVisualizationInfo, new EnumMap<>(PlayerRole.class));
	}

	public Function<String, Map<Parameter, Object>> getGameParamMapFactory() {
		return gameParamMapFactory;
	}

	public Object getParameterValue(final Parameter param) {
		final Properties props = loadClassProps();
		return param.parseValue(props);
	}

}

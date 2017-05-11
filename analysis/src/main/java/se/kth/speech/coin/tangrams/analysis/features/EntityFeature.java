/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
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
package se.kth.speech.coin.tangrams.analysis.features;

import java.awt.Color;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.ToIntFunction;

import se.kth.speech.IntArrays;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;
import weka.core.Attribute;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @see <a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.
 *
 */
public enum EntityFeature {
	BLUE, BRIGHTNESS, EDGE_COUNT, GREEN, HUE, POSITION_X, POSITION_Y, RED, SATURATION, SHAPE, SIZE;

	public static final class Extractor extends AbstractInstanceFeatureExtractor<EntityFeature, Extractor.Context> {

		public static final class Context {

			private final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum;

			private final SpatialRegion pieceRegion;

			private final int[] modelDims;

			private final double modelArea;

			private final ToIntFunction<? super String> namedResourceEdgeCountFactory;

			public Context(final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum,
					final SpatialRegion pieceRegion, final int[] modelDims, final double modelArea,
					final ToIntFunction<? super String> namedResourceEdgeCountFactory) {
				this.pieceImgVizInfoDatum = pieceImgVizInfoDatum;
				this.pieceRegion = pieceRegion;
				this.modelDims = modelDims;
				this.modelArea = modelArea;
				this.namedResourceEdgeCountFactory = namedResourceEdgeCountFactory;
			}
		}

		private static final String DEFAULT_ATTR_NAME_PREFIX = "";

		private static final List<EntityFeature> DEFAULT_ORDERING;

		static {
			DEFAULT_ORDERING = Arrays.asList(SHAPE, EDGE_COUNT, RED, GREEN, BLUE, HUE, SATURATION, BRIGHTNESS, SIZE,
					POSITION_X, POSITION_Y);
			assert DEFAULT_ORDERING.size() == EntityFeature.values().length;
		}

		private static Map<EntityFeature, Attribute> createFeatureAttrMap(final Iterable<EntityFeature> features,
				final List<String> shapeVals) {
			return createFeatureAttrMap(features, DEFAULT_ATTR_NAME_PREFIX, shapeVals);
		}

		private static Map<EntityFeature, Attribute> createFeatureAttrMap(final Iterable<EntityFeature> features,
				final String attrNamePrefix, final List<String> shapeVals) {
			final Map<EntityFeature, Function<String, Attribute>> attrFactories = createFeatureTypedAttrFactoryMap(
					shapeVals);
			final Map<EntityFeature, Attribute> result = new EnumMap<>(EntityFeature.class);
			for (final EntityFeature feature : features) {
				final Function<String, Attribute> attrFactory = attrFactories.get(feature);
				result.put(feature, attrFactory.apply(attrNamePrefix + feature.name()));
			}
			return result;
		}

		private static Map<EntityFeature, Attribute> createFeatureAttrMap(final List<String> shapeVals) {
			return createFeatureAttrMap(DEFAULT_ATTR_NAME_PREFIX, shapeVals);
		}

		private static Map<EntityFeature, Attribute> createFeatureAttrMap(final String attrNamePrefix,
				final List<String> shapeVals) {
			return createFeatureAttrMap(EnumSet.allOf(EntityFeature.class), attrNamePrefix, shapeVals);
		}

		private static Map<EntityFeature, Function<String, Attribute>> createFeatureTypedAttrFactoryMap(
				final List<String> shapeVals) {
			final Map<EntityFeature, Function<String, Attribute>> result = new EnumMap<>(EntityFeature.class);
			final Function<String, Attribute> doubleVal = name -> new Attribute(name);
			result.put(EntityFeature.BLUE, doubleVal);
			result.put(EntityFeature.BRIGHTNESS, doubleVal);
			result.put(EntityFeature.EDGE_COUNT, doubleVal);
			result.put(EntityFeature.GREEN, doubleVal);
			result.put(EntityFeature.HUE, doubleVal);
			result.put(EntityFeature.POSITION_X, doubleVal);
			result.put(EntityFeature.POSITION_Y, doubleVal);
			result.put(EntityFeature.RED, doubleVal);
			result.put(EntityFeature.SATURATION, doubleVal);
			result.put(EntityFeature.SHAPE, name -> new Attribute(name, shapeVals));
			result.put(EntityFeature.SIZE, doubleVal);
			assert result.size() == EntityFeature.values().length;
			return result;
		}

		public Extractor(final Iterable<EntityFeature> features, final List<String> shapeVals) {
			this(createFeatureAttrMap(features, shapeVals));
		}

		public Extractor(final Iterable<EntityFeature> features, final String attrNamePrefix,
				final List<String> shapeVals) {
			this(createFeatureAttrMap(features, attrNamePrefix, shapeVals));
		}

		public Extractor(final List<String> shapeVals) {
			this(createFeatureAttrMap(shapeVals));
		}

		public Extractor(final Map<EntityFeature, Attribute> featureAttrs) {
			super(featureAttrs);
		}

		public Extractor(final String attrNamePrefix, final List<String> shapeVals) {
			this(createFeatureAttrMap(attrNamePrefix, shapeVals));
		}

		@Override
		public Optional<Object> getVal(final EntityFeature feature, final Context context) {
			final Color color = context.pieceImgVizInfoDatum.getColor();
			final float[] hsbVals = Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
			final Object val;
			switch (feature) {
			case RED:
				val = color.getRed();
				break;
			case GREEN:
				val = color.getGreen();
				break;
			case BLUE:
				val = color.getBlue();
				break;
			case HUE:
				val = hsbVals[0];
				break;
			case SATURATION:
				val = hsbVals[1];
				break;
			case BRIGHTNESS:
				val = hsbVals[2];
				break;
			case POSITION_X: {
				final SpatialRegion r = context.pieceRegion;
				final double centerX = r.getXLowerBound() + r.getLengthX() / 2.0;
				final double posX = centerX / context.modelDims[0];
				val = posX;
				break;
			}
			case POSITION_Y: {
				final SpatialRegion r = context.pieceRegion;
				final double centerY = r.getYLowerBound() + r.getLengthY() / 2.0;
				final double posY = centerY / context.modelDims[1];
				val = posY;
				break;
			}
			case EDGE_COUNT: {
				final String imgResName = context.pieceImgVizInfoDatum.getResourceName();
				final int edgeCount = context.namedResourceEdgeCountFactory.applyAsInt(imgResName);
				val = edgeCount;
				break;
			}
			case SHAPE:
				final String imgResName = context.pieceImgVizInfoDatum.getResourceName();
				val = imgResName;
				break;
			case SIZE:
				final int pieceArea = IntArrays.product(context.pieceRegion.getDimensions());
				final double sizeFeatureVal = pieceArea / context.modelArea;
				val = sizeFeatureVal;
				break;
			default: {
				throw new AssertionError("Missing enum-handling logic.");
			}
			}
			return Optional.of(val);
		}

	}

}
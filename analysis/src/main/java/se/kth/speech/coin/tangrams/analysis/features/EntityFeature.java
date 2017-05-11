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
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.function.ToIntFunction;

import se.kth.speech.IntArrays;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;
import weka.core.Attribute;
import weka.core.Instance;

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

	public static final class Extractor {

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

		private final Map<EntityFeature, Attribute> featureAttrs;

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
			this.featureAttrs = featureAttrs;
		}

		public Extractor(final String attrNamePrefix, final List<String> shapeVals) {
			this(createFeatureAttrMap(attrNamePrefix, shapeVals));
		}

		public Map<EntityFeature, Attribute> getFeatureAttrs() {
			return Collections.unmodifiableMap(featureAttrs);
		}

		public Object getVal(final EntityFeature feature,
				final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum, final SpatialRegion pieceRegion,
				final int[] modelDims, final double modelArea,
				final ToIntFunction<? super String> namedResourceEdgeCountFactory) {
			final Color color = pieceImgVizInfoDatum.getColor();
			final float[] hsbVals = Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
			final Object result;
			switch (feature) {
			case RED:
				result = color.getRed();
				break;
			case GREEN:
				result = color.getGreen();
				break;
			case BLUE:
				result = color.getBlue();
				break;
			case HUE:
				result = hsbVals[0];
				break;
			case SATURATION:
				result = hsbVals[1];
				break;
			case BRIGHTNESS:
				result = hsbVals[2];
				break;
			case POSITION_X: {
				final double centerX = pieceRegion.getXLowerBound() + pieceRegion.getLengthX() / 2.0;
				final double posX = centerX / modelDims[0];
				result = posX;
				break;
			}
			case POSITION_Y: {
				final double centerY = pieceRegion.getYLowerBound() + pieceRegion.getLengthY() / 2.0;
				final double posY = centerY / modelDims[1];
				result = posY;
				break;
			}
			case EDGE_COUNT: {
				final String imgResName = pieceImgVizInfoDatum.getResourceName();
				final int edgeCount = namedResourceEdgeCountFactory.applyAsInt(imgResName);
				result = edgeCount;
				break;
			}
			case SHAPE:
				final String imgResName = pieceImgVizInfoDatum.getResourceName();
				result = imgResName;
				break;
			case SIZE:
				final int pieceArea = IntArrays.product(pieceRegion.getDimensions());
				final double sizeFeatureVal = pieceArea / modelArea;
				result = sizeFeatureVal;
				break;
			default: {
				throw new AssertionError("Missing enum-handling logic.");
			}
			}
			return result;
		}

		void setVals(final Instance vals, final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum,
				final SpatialRegion pieceRegion, final int[] modelDims, final double modelArea,
				final ToIntFunction<? super String> namedResourceEdgeCountFactory) {
			for (final Entry<EntityFeature, Attribute> featureAttr : featureAttrs.entrySet()) {
				final EntityFeature feature = featureAttr.getKey();
				final Attribute attr = featureAttr.getValue();
				final Object val = getVal(feature, pieceImgVizInfoDatum, pieceRegion, modelDims, modelArea,
						namedResourceEdgeCountFactory);
				if (val instanceof Number) {
					vals.setValue(attr, ((Number) val).doubleValue());
				} else {
					vals.setValue(attr, val.toString());
				}
			}
		}

	}

}
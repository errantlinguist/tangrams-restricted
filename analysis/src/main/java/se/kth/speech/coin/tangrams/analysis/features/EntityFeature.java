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
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.DoubleStream;

import it.unimi.dsi.fastutil.objects.Object2DoubleMap;
import se.kth.speech.IntArrays;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;

enum EntityFeature {
	COLOR, POSITION_X, POSITION_Y, SHAPE, SIZE;

	private static final double NULL_FEATURE_VAL = -1.0;

	private static final List<EntityFeature> ORDERING;

	private static final Object2DoubleMap<String> SHAPE_FEATURE_VALS = createShapeFeatureValueMap();

	static {
		ORDERING = Arrays.asList(SHAPE, COLOR, SIZE, POSITION_X, POSITION_Y);
		assert ORDERING.size() == EntityFeature.values().length;
	}

	private static float createColorFeatureVal(final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum) {
		final Color color = pieceImgVizInfoDatum.getColor();
		final float[] hsbVals = Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
		return hsbVals[0];
	}

	private static Object2DoubleMap<String> createShapeFeatureValueMap() {
		final Set<String> possibleShapeStrValues = IconImages.getImageResources().keySet();
		return FeatureMaps.createOrdinalFeatureValMap(possibleShapeStrValues);
	}

	private static double getShapeFeatureVal(final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum) {
		final String strVal = pieceImgVizInfoDatum.getResourceName();
		return SHAPE_FEATURE_VALS.getDouble(strVal);
	}

	/**
	 * @return the ordering
	 */
	static List<EntityFeature> getOrdering() {
		return Collections.unmodifiableList(ORDERING);
	}

	static void setVals(final DoubleStream.Builder vals,
			final ImageVisualizationInfoDescription.Datum pieceImgVizInfoDatum, final SpatialRegion pieceRegion,
			final int[] modelDims, final double modelArea) {
		for (final EntityFeature feature : ORDERING) {
			switch (feature) {
			case COLOR:
				final float colorFeatureVal = createColorFeatureVal(pieceImgVizInfoDatum);
				vals.accept(colorFeatureVal);
				break;
			case POSITION_X: {
				final double centerX = pieceRegion.getXLowerBound() + pieceRegion.getLengthX() / 2.0;
				final double posX = centerX / modelDims[0];
				vals.accept(posX);
				break;
			}
			case POSITION_Y: {
				final double centerY = pieceRegion.getYLowerBound() + pieceRegion.getLengthY() / 2.0;
				final double posY = centerY / modelDims[1];
				vals.accept(posY);
				break;
			}
			case SHAPE:
				final double shapeFeatureVal = getShapeFeatureVal(pieceImgVizInfoDatum);
				vals.accept(shapeFeatureVal);
				break;
			case SIZE:
				final int pieceArea = IntArrays.product(pieceRegion.getDimensions());
				final double sizeFeatureVal = pieceArea / modelArea;
				vals.accept(sizeFeatureVal);
				break;
			default: {
				throw new AssertionError("Missing enum-handling logic.");
			}
			}
		}
	}

	static void setVals(final DoubleStream.Builder vals,
			final Optional<Entry<ImageVisualizationInfoDescription.Datum, SpatialRegion>> entityData,
			final int[] modelDims, final double modelArea) {
		if (entityData.isPresent()) {
			final Entry<ImageVisualizationInfoDescription.Datum, SpatialRegion> data = entityData.get();
			setVals(vals, data.getKey(), data.getValue(), modelDims, modelArea);
		} else {
			// Set null feature vals
			ORDERING.stream().mapToDouble(feature -> NULL_FEATURE_VAL).forEach(vals);
		}
	}
}
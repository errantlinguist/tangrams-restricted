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
package se.kth.speech.coin.tangrams.analysis.features;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.ToIntFunction;

import se.kth.speech.IntArrays;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @see
 *      <ul>
 *      <li><a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen (2015). &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.</li>
 *      <li><a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *      &amp; David Schlangen (2015). &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em><a>.</li>
 *      </ul>
 *
 */
public enum EntityFeature {
	ALPHA, BLUE, BRIGHTNESS, EDGE_COUNT, GREEN, HUE, POSITION_X, POSITION_Y, RED, SATURATION, SHAPE, SIZE;

	public static final class Extractor implements FeatureExtractor<EntityFeature, Extractor.Context> {

		public static final class Context {

			private final double modelArea;

			private final int[] modelDims;

			private final ToIntFunction<? super String> namedResourceEdgeCountFactory;

			private final ImageVisualizationInfo.Datum pieceImgVizInfoDatum;

			private final SpatialRegion pieceRegion;

			Context(final ImageVisualizationInfo.Datum pieceImgVizInfoDatum, final SpatialRegion pieceRegion,
					final int[] modelDims, final double modelArea,
					final ToIntFunction<? super String> namedResourceEdgeCountFactory) {
				this.pieceImgVizInfoDatum = pieceImgVizInfoDatum;
				this.pieceRegion = pieceRegion;
				this.modelDims = modelDims;
				this.modelArea = modelArea;
				this.namedResourceEdgeCountFactory = namedResourceEdgeCountFactory;
			}

			/*
			 * (non-Javadoc)
			 *
			 * @see java.lang.Object#equals(java.lang.Object)
			 */
			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}
				if (obj == null) {
					return false;
				}
				if (!(obj instanceof Context)) {
					return false;
				}
				final Context other = (Context) obj;
				if (Double.doubleToLongBits(modelArea) != Double.doubleToLongBits(other.modelArea)) {
					return false;
				}
				if (!Arrays.equals(modelDims, other.modelDims)) {
					return false;
				}
				if (namedResourceEdgeCountFactory == null) {
					if (other.namedResourceEdgeCountFactory != null) {
						return false;
					}
				} else if (!namedResourceEdgeCountFactory.equals(other.namedResourceEdgeCountFactory)) {
					return false;
				}
				if (pieceImgVizInfoDatum == null) {
					if (other.pieceImgVizInfoDatum != null) {
						return false;
					}
				} else if (!pieceImgVizInfoDatum.equals(other.pieceImgVizInfoDatum)) {
					return false;
				}
				if (pieceRegion == null) {
					if (other.pieceRegion != null) {
						return false;
					}
				} else if (!pieceRegion.equals(other.pieceRegion)) {
					return false;
				}
				return true;
			}

			/*
			 * (non-Javadoc)
			 *
			 * @see java.lang.Object#hashCode()
			 */
			@Override
			public int hashCode() {
				final int prime = 31;
				int result = 1;
				long temp;
				temp = Double.doubleToLongBits(modelArea);
				result = prime * result + (int) (temp ^ temp >>> 32);
				result = prime * result + Arrays.hashCode(modelDims);
				result = prime * result
						+ (namedResourceEdgeCountFactory == null ? 0 : namedResourceEdgeCountFactory.hashCode());
				result = prime * result + (pieceImgVizInfoDatum == null ? 0 : pieceImgVizInfoDatum.hashCode());
				result = prime * result + (pieceRegion == null ? 0 : pieceRegion.hashCode());
				return result;
			}

			/*
			 * (non-Javadoc)
			 *
			 * @see java.lang.Object#toString()
			 */
			@Override
			public String toString() {
				final StringBuilder builder = new StringBuilder(256);
				builder.append("Context [modelArea=");
				builder.append(modelArea);
				builder.append(", modelDims=");
				builder.append(Arrays.toString(modelDims));
				builder.append(", namedResourceEdgeCountFactory=");
				builder.append(namedResourceEdgeCountFactory);
				builder.append(", pieceImgVizInfoDatum=");
				builder.append(pieceImgVizInfoDatum);
				builder.append(", pieceRegion=");
				builder.append(pieceRegion);
				builder.append(']');
				return builder.toString();
			}
		}

		@Override
		public Optional<Object> apply(final EntityFeature feature, final Context context) {
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
			case ALPHA:
				val = color.getAlpha();
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

	private static final List<EntityFeature> CANONICAL_ORDERING;

	static {
		CANONICAL_ORDERING = Collections.unmodifiableList(Arrays.asList(SHAPE, EDGE_COUNT, SIZE, RED, GREEN, BLUE,
				ALPHA, HUE, SATURATION, BRIGHTNESS, POSITION_X, POSITION_Y));
		assert CANONICAL_ORDERING.size() == EntityFeature.values().length;
	}

	public static List<EntityFeature> getCanonicalOrdering() {
		return CANONICAL_ORDERING;
	}

}
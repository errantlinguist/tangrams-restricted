/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.content;

import java.util.Arrays;
import java.util.function.Function;
import java.util.function.IntSupplier;

import se.kth.speech.MathDivisors;

public final class ImageViewInfo {

	public enum Orientation {
		LANDSCAPE, PORTRAIT, SQUARE;

		static Orientation getOrientation(final int width, final int height) {
			final Orientation result;
			if (width < height) {
				result = Orientation.PORTRAIT;
			} else if (height < width) {
				result = Orientation.LANDSCAPE;
			} else {
				result = Orientation.SQUARE;
			}
			return result;
		}
	}

	public static final class RasterizationInfo {

		private static int getGcd(final int width, final int height) {
			return MathDivisors.gcd(width, height);
		}

		private final IntSupplier heightGetter;

		private final IntSupplier widthGetter;

		RasterizationInfo(final IntSupplier widthGetter, final IntSupplier heightGetter) {
			this.widthGetter = widthGetter;
			this.heightGetter = heightGetter;
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
			if (!(obj instanceof RasterizationInfo)) {
				return false;
			}
			final RasterizationInfo other = (RasterizationInfo) obj;
			if (heightGetter == null) {
				if (other.heightGetter != null) {
					return false;
				}
			} else if (!heightGetter.equals(other.heightGetter)) {
				return false;
			}
			if (widthGetter == null) {
				if (other.widthGetter != null) {
					return false;
				}
			} else if (!widthGetter.equals(other.widthGetter)) {
				return false;
			}
			return true;
		}

		public int[] getAspectRatio() {
			final int width = getWidth();
			final int height = getHeight();
			final int gcd = getGcd(width, height);
			return new int[] { width / gcd, height / gcd };
		}

		public int getGcd() {
			return getGcd(getWidth(), getHeight());
		}

		public int getHeight() {
			return heightGetter.getAsInt();
		}

		public Orientation getOrientation() {
			return Orientation.getOrientation(getWidth(), getHeight());
		}

		public int getWidth() {
			return widthGetter.getAsInt();
		}

		public int getWidthHeightQuotient() {
			return widthGetter.getAsInt() / heightGetter.getAsInt();
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
			result = prime * result + (heightGetter == null ? 0 : heightGetter.hashCode());
			result = prime * result + (widthGetter == null ? 0 : widthGetter.hashCode());
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder();
			builder.append("RasterizationInfo [getWidth()=");
			builder.append(getWidth());
			builder.append(", getHeight()=");
			builder.append(getHeight());
			builder.append(", getOrientation()=");
			builder.append(getOrientation());
			builder.append(", getWidthHeightQuotient()=");
			builder.append(getWidthHeightQuotient());
			builder.append(", getAspectRatio()=");
			builder.append(Arrays.toString(getAspectRatio()));
			builder.append(", getGcd()=");
			builder.append(getGcd());
			builder.append(']');
			return builder.toString();
		}
	}

	private final RasterizationInfo rasterization;

	private final ImageVisualizationInfo visualization;

	ImageViewInfo(final ImageVisualizationInfo visualization, final RasterizationInfo rasterization) {
		this.visualization = visualization;
		this.rasterization = rasterization;
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
		if (!(obj instanceof ImageViewInfo)) {
			return false;
		}
		final ImageViewInfo other = (ImageViewInfo) obj;
		if (rasterization == null) {
			if (other.rasterization != null) {
				return false;
			}
		} else if (!rasterization.equals(other.rasterization)) {
			return false;
		}
		if (visualization == null) {
			if (other.visualization != null) {
				return false;
			}
		} else if (!visualization.equals(other.visualization)) {
			return false;
		}
		return true;
	}

	public int[] getGridSize(final Function<ImageSize, Integer> sizeFactors) {
		final int factor = sizeFactors.apply(getVisualization().getSize());
		final int[] aspectRatio = getRasterization().getAspectRatio();
		// NOTE: Grid rows go top-bottom and cols go left-right
		return new int[] { aspectRatio[1] * factor, aspectRatio[0] * factor };
	}

	/**
	 * @return the rasterization
	 */
	public RasterizationInfo getRasterization() {
		return rasterization;
	}

	/**
	 * @return the visualization
	 */
	public ImageVisualizationInfo getVisualization() {
		return visualization;
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
		result = prime * result + (rasterization == null ? 0 : rasterization.hashCode());
		result = prime * result + (visualization == null ? 0 : visualization.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("ImageViewInfo [rasterization=");
		builder.append(rasterization);
		builder.append(", visualization=");
		builder.append(visualization);
		builder.append(']');
		return builder.toString();
	}
}
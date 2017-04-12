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

import java.awt.Color;
import java.net.URL;
import java.util.Collections;
import java.util.List;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class ImageVisualizationInfo {

	public static final class Datum {
		private final Color color;

		private final URL resourceLoc;

		private final ImageSize size;

		public Datum(final URL resourceLoc, final Color color, final ImageSize size) {
			this.resourceLoc = resourceLoc;
			this.color = color;
			this.size = size;
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
			if (!(obj instanceof Datum)) {
				return false;
			}
			final Datum other = (Datum) obj;
			if (color == null) {
				if (other.color != null) {
					return false;
				}
			} else if (!color.equals(other.color)) {
				return false;
			}
			if (resourceLoc == null) {
				if (other.resourceLoc != null) {
					return false;
				}
			} else if (!resourceLoc.equals(other.resourceLoc)) {
				return false;
			}
			if (size != other.size) {
				return false;
			}
			return true;
		}

		/**
		 * @return the color
		 */
		public Color getColor() {
			return color;
		}

		/**
		 * @return the resourceLoc
		 */
		public URL getResourceLoc() {
			return resourceLoc;
		}

		/**
		 * @return the size
		 */
		public ImageSize getSize() {
			return size;
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
			result = prime * result + (color == null ? 0 : color.hashCode());
			result = prime * result + (resourceLoc == null ? 0 : resourceLoc.hashCode());
			result = prime * result + (size == null ? 0 : size.hashCode());
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
			builder.append("Datum [resourceLoc=");
			builder.append(resourceLoc);
			builder.append(", color=");
			builder.append(color);
			builder.append(", size=");
			builder.append(size);
			builder.append(']');
			return builder.toString();
		}

	}

	private final List<Datum> data;

	private final int uniqueImgResourceCount;

	public ImageVisualizationInfo(final List<Datum> data, final int uniqueImgResourceCount) {
		this.data = data;
		this.uniqueImgResourceCount = uniqueImgResourceCount;
	}

	/**
	 * @return the data
	 */
	public List<Datum> getData() {
		return Collections.unmodifiableList(data);
	}

	/**
	 * @return the uniqueImgResourceCount
	 */
	public int getUniqueImageResourceCount() {
		return uniqueImgResourceCount;
	}

}

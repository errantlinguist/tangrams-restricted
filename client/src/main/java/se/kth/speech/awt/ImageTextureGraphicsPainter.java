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
package se.kth.speech.awt;

import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.image.BufferedImage;
import java.util.function.Consumer;

public final class ImageTextureGraphicsPainter implements Consumer<Graphics2D> {

	private final Component component;

	private final BufferedImage textureImg;

	public ImageTextureGraphicsPainter(final Component component, final BufferedImage textureImg) {
		this.component = component;
		this.textureImg = textureImg;
	}

	@Override
	public void accept(final Graphics2D g2d) {
		final TexturePaint diagPaint = new TexturePaint(textureImg,
				new Rectangle(0, 0, textureImg.getWidth(), textureImg.getHeight()));
		g2d.setPaint(diagPaint);
		g2d.fillRect(0, 0, component.getWidth(), component.getHeight());
		g2d.dispose();
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
		if (!(obj instanceof ImageTextureGraphicsPainter)) {
			return false;
		}
		final ImageTextureGraphicsPainter other = (ImageTextureGraphicsPainter) obj;
		if (component == null) {
			if (other.component != null) {
				return false;
			}
		} else if (!component.equals(other.component)) {
			return false;
		}
		if (textureImg == null) {
			if (other.textureImg != null) {
				return false;
			}
		} else if (!textureImg.equals(other.textureImg)) {
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
		result = prime * result + (component == null ? 0 : component.hashCode());
		result = prime * result + (textureImg == null ? 0 : textureImg.hashCode());
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
		builder.append("ImageTextureGraphicsPainter [component=");
		builder.append(component);
		builder.append(", textureImg=");
		builder.append(textureImg);
		builder.append("]");
		return builder.toString();
	}

}
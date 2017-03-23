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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import javax.swing.Icon;

/**
 * @author <a href=
 *         "https://stackoverflow.com/users/230513/trashgod">trashgod</a>
 * @since 9 Jan 2017
 * @see <a href="http://stackoverflow.com/a/3072979/1391325">SO answer</a>
 *
 */
public final class ColorIcon implements Icon {

	private Color color;

	private int iconHeight;

	private int iconWidth;

	public ColorIcon(final int iconWidth, final int iconHeight, final Color color) {
		this.iconWidth = iconWidth;
		this.iconHeight = iconHeight;
		this.color = color;
	}

	/**
	 * @return the color
	 */
	public Color getColor() {
		return color;
	}

	@Override
	public int getIconHeight() {
		return iconHeight;
	}

	@Override
	public int getIconWidth() {
		return iconWidth;
	}

	/**
	 * @return the iconWidth
	 */
	public int getWidth() {
		return iconWidth;
	}

	@Override
	public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
		final Graphics2D g2d = (Graphics2D) g;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g2d.setColor(color);
		g2d.fillOval(x, y, iconWidth, iconHeight);
	}

	/**
	 * @param color
	 *            the color to set
	 */
	public void setColor(final Color color) {
		this.color = color;
	}

	/**
	 * @param iconHeight
	 *            the iconHeight to set
	 */
	public void setIconHeight(final int iconHeight) {
		this.iconHeight = iconHeight;
	}

	/**
	 * @param iconWidth
	 *            the iconWidth to set
	 */
	public void setIconWidth(final int iconWidth) {
		this.iconWidth = iconWidth;
	}
}
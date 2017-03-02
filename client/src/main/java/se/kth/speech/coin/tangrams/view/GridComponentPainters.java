/*
 *  This file is part of se.kth.speech.coin.tangrams.client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.view;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.IntFunction;

import se.kth.speech.awt.ImageTextureGraphicsPainter;
import se.kth.speech.coin.tangrams.game.Model;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 8 Feb 2017
 *
 */
public final class GridComponentPainters {

	public static Consumer<Graphics> createComponentTextureGraphicsPainter(final int imgId, final Component component,
			final IntFunction<? extends BufferedImage> imgFactory) {
		final Consumer<Graphics> result;
		final BufferedImage img = imgFactory.apply(imgId);
		if (img == null) {
			result = g -> {
				// Do nothing
			};
		} else {
			final Consumer<Graphics2D> painter = new ImageTextureGraphicsPainter(component, img);
			result = g -> painter.accept((Graphics2D) g);
		}
		return result;
	}

	public static <C extends Component> BiFunction<C, Integer, Consumer<Graphics>> createRowBackgroundTextureGraphicsPainterFactory(
			final Model<?> model, final IntFunction<? extends BufferedImage> rowBackgroundImageFactory) {
		return (component, coordOccupantArrayIdx) -> createComponentTextureGraphicsPainter(
				model.getCoordinatePoint(coordOccupantArrayIdx)[0], component, rowBackgroundImageFactory);
	}

	private GridComponentPainters() {
	}

}

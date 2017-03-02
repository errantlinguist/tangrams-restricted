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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import javax.swing.ImageIcon;
import javax.swing.JLabel;

import se.kth.speech.awt.ScaledImageFactory;
import se.kth.speech.coin.tangrams.game.Model;

public final class RowBackgroundPaintedModelCoordinateGridPanelFactory<T>
		implements BiFunction<Model<T>, Dimension, ModelCoordinateGridPanel<?>> {

	private class ScaledCoordLabelFactory implements Function<T, JLabel> {

		private final Function<? super T, ? extends Image> coordOccupantImageFactory;
		
		private final Function<Image, Image> scaledImgFactory;

		private ScaledCoordLabelFactory(final Model<T> model, final Dimension panelSize,
				final Function<? super T, ? extends Image> coordOccupantImageFactory) {
			this.coordOccupantImageFactory = coordOccupantImageFactory;
			this.scaledImgFactory = new ScaledImageFactory(Image.SCALE_SMOOTH, panelSize,
					model.getCoordinateDimensions());
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		public JLabel apply(final T coordOccupant) {
			JLabel result;
			final Image origImg = coordOccupantImageFactory.apply(coordOccupant);
			if (origImg == null) {
				result = null;
			} else {
				final Image scaledImg = scaledImgFactory.apply(origImg);
				result = new JLabel(new ImageIcon(scaledImg));
			}
			return result;
		}

	}

	private final Function<? super T, ? extends Image> coordOccupantImageFactory;

	private final IntFunction<? extends BufferedImage> rowBackgroundImageFactory;

	public RowBackgroundPaintedModelCoordinateGridPanelFactory(
			final Function<? super T, ? extends Image> coordOccupantImageFactory,
			final IntFunction<? extends BufferedImage> rowBackgroundImageFactory) {
		this.coordOccupantImageFactory = coordOccupantImageFactory;
		this.rowBackgroundImageFactory = rowBackgroundImageFactory;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public ModelCoordinateGridPanel<?> apply(final Model<T> model, final Dimension panelSize) {
		final Supplier<PlayerHighlightBorderFactory> highlightBorderFactorySupplier = PlayerHighlightBorderFactory::new;
		final BiFunction<ModelCoordinatePanel<PlayerHighlightBorderFactory>, Integer, Consumer<Graphics>> gridBackgroundPainterFactory = GridComponentPainters
				.createRowBackgroundTextureGraphicsPainterFactory(model, rowBackgroundImageFactory);
		final ModelCoordinatePanelFactory<T, PlayerHighlightBorderFactory> gridPanelFactory = new ModelCoordinatePanelFactory<>(
				model, highlightBorderFactorySupplier, gridBackgroundPainterFactory,
				new ScaledCoordLabelFactory(model, panelSize, coordOccupantImageFactory));
		final List<ModelCoordinatePanel<PlayerHighlightBorderFactory>> gridPanels = gridPanelFactory.get()
				.collect(Collectors.toList());
		return new ModelCoordinateGridPanel<>(model, panelSize, gridPanels);
	}

}
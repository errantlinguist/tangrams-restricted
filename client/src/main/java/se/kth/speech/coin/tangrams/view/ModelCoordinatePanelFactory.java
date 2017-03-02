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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.border.Border;

import se.kth.speech.coin.tangrams.game.Model;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 15 Feb 2017
 *
 */
final class ModelCoordinatePanelFactory<T, PHBF extends Consumer<? super String> & Supplier<? extends Border>>
		implements Supplier<Stream<ModelCoordinatePanel<PHBF>>> {

	private static final Border DEFAULT_GRID_PANEL_BORDER = BorderFactory.createMatteBorder(1, 1, 1, 1, Color.GRAY);

	private final Function<T, JLabel> coordLabelFactory;

	private final Supplier<PHBF> highlightBorderFactorySupplier;

	private final BiFunction<? super ModelCoordinatePanel<PHBF>, Integer, Consumer<Graphics>> gridBackgroundPainterFactory;

	private final Model<T> model;

	ModelCoordinatePanelFactory(final Model<T> model, final Supplier<PHBF> highlightBorderFactorySupplier,
			final BiFunction<? super ModelCoordinatePanel<PHBF>, Integer, Consumer<Graphics>> gridBackgroundPainterFactory,
			final Function<T, JLabel> coordLabelFactory) {
		this.model = model;
		this.highlightBorderFactorySupplier = highlightBorderFactorySupplier;
		this.gridBackgroundPainterFactory = gridBackgroundPainterFactory;
		this.coordLabelFactory = coordLabelFactory;
	}

	@Override
	public Stream<ModelCoordinatePanel<PHBF>> get() {
		final T[] coordOccupants = model.getCoordinateOccupants().getValues();
		return IntStream.range(0, coordOccupants.length).mapToObj(coordOccupantArrayIdx -> {
			final T coordOccupant = coordOccupants[coordOccupantArrayIdx];
			final int[] modelCoords = model.getCoordinatePoint(coordOccupantArrayIdx);
			final PHBF borderFactory = highlightBorderFactorySupplier.get();
			final ModelCoordinatePanel<PHBF> result = new ModelCoordinatePanel<>(modelCoords, new BorderLayout(),
					DEFAULT_GRID_PANEL_BORDER, borderFactory);
			final Consumer<Graphics> rowBackgroundImgPainter = gridBackgroundPainterFactory.apply(result,
					coordOccupantArrayIdx);
			// NOTE: It's not possible to pass this directly to the
			// constructor because it needs a reference to the JComponent
			// object already
			result.setDelegateComponentPainter(rowBackgroundImgPainter);

			final JLabel coordLabel = coordLabelFactory.apply(coordOccupant);
			if (coordLabel != null) {
				result.add(coordLabel);
			}
			return result;
		});
	}

}

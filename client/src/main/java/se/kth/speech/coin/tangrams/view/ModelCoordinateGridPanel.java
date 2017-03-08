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
package se.kth.speech.coin.tangrams.view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;

import se.kth.speech.coin.tangrams.game.Model;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
public final class ModelCoordinateGridPanel<PHBF extends Consumer<? super String> & Supplier<? extends Border>> extends JPanel {

	private static class GridSquarePanelFactory<T, PHBF extends Consumer<? super String> & Supplier<? extends Border>>
			implements Function<Model<T>, Stream<ModelCoordinatePanel<PHBF>>> {

		private final Function<? super T, ? extends JLabel> coordLabelFactory;

		private final BiFunction<? super ModelCoordinatePanel<PHBF>, Integer, Consumer<Graphics>> gridBackgroundPainterFactory;

		private final Supplier<PHBF> highlightBorderFactorySupplier;

		private GridSquarePanelFactory(final Model<T> model, final Supplier<PHBF> highlightBorderFactorySupplier,
				final BiFunction<? super ModelCoordinatePanel<PHBF>, Integer, Consumer<Graphics>> gridBackgroundPainterFactory,
				final Function<? super T, ? extends JLabel> coordLabelFactory) {
			this.highlightBorderFactorySupplier = highlightBorderFactorySupplier;
			this.gridBackgroundPainterFactory = gridBackgroundPainterFactory;
			this.coordLabelFactory = coordLabelFactory;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		public Stream<ModelCoordinatePanel<PHBF>> apply(final Model<T> model) {
			final List<T> coordOccupants = model.getCoordinateOccupants().getValues();
			return IntStream.range(0, coordOccupants.size()).mapToObj(coordOccupantArrayIdx -> {
				final T coordOccupant = coordOccupants.get(coordOccupantArrayIdx);
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

	private static final Border DEFAULT_GRID_PANEL_BORDER = BorderFactory.createMatteBorder(1, 1, 1, 1, Color.GRAY);

	/**
	 *
	 */
	private static final long serialVersionUID = -3759792731112624486L;

	private static <T, PHBF extends Consumer<? super String> & Supplier<? extends Border>> Stream<ModelCoordinatePanel<PHBF>> createGridPanels(
			final Model<T> model, final Function<T, JLabel> coordLabelFactory,
			final BiFunction<? super ModelCoordinatePanel<PHBF>, Integer, Consumer<Graphics>> gridBackgroundPainterFactory,
			final Supplier<PHBF> highlightBorderFactorySupplier) {
		final GridSquarePanelFactory<T, PHBF> factory = new GridSquarePanelFactory<>(model,
				highlightBorderFactorySupplier, gridBackgroundPainterFactory, coordLabelFactory);
		return factory.apply(model);
	}

	private final GridLayout gridLayout;

	private final List<ModelCoordinatePanel<PHBF>> gridPanels;

	<T> ModelCoordinateGridPanel(
			final Model<T> model, final Dimension boardSize,
			final Function<T, JLabel> coordLabelFactory,
			final Supplier<PHBF> highlightBorderFactorySupplier,
			final BiFunction<? super ModelCoordinatePanel<PHBF>, Integer, Consumer<Graphics>> gridBackgroundPainterFactory) {
		this(model, boardSize, createGridPanels(model, coordLabelFactory,
				gridBackgroundPainterFactory, highlightBorderFactorySupplier).collect(Collectors.toList()));
	}
	
	<T> ModelCoordinateGridPanel(
			final Model<T> model, final Dimension boardSize, final List<ModelCoordinatePanel<PHBF>> gridPanels) {
		final int[] coordDims = model.getCoordinateDimensions();
		gridLayout = new GridLayout(coordDims[0], coordDims[1]);
		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		setLayout(gridLayout);
		setPreferredSize(boardSize);
		setBounds(0, 0, boardSize.width, boardSize.height);
		this.gridPanels = gridPanels;
		gridPanels.forEach(this::add);
	}

	public ModelCoordinatePanel<PHBF> getCoordPanel(final int[] modelCoords) {
		final int gridArraylIdx = getGridArrayIdx(modelCoords[0], modelCoords[1]);
		return gridPanels.get(gridArraylIdx);
	}

	private int getGridArrayIdx(final int x, final int y) {
		final int rowStartIdx = x * gridLayout.getColumns();
		return rowStartIdx + y;
	}

}
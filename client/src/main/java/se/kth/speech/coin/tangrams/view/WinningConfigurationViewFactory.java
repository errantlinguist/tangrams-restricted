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

import java.awt.Dimension;
import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Function;

import javax.swing.JFrame;

import se.kth.speech.coin.tangrams.game.Model;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Jan 2017
 *
 */
final class WinningConfigurationViewFactory<T> implements Function<Model<T>, JFrame> {

	private final BiFunction<? super Model<T>, ? super Dimension, ? extends ModelCoordinateGridPanel<?>> gridPanelFactory;

	private final int gridPanelSize;

	WinningConfigurationViewFactory(
			final BiFunction<? super Model<T>, ? super Dimension, ? extends ModelCoordinateGridPanel<?>> gridPanelFactory,
			final int gridPanelSize) {
		this.gridPanelFactory = gridPanelFactory;
		this.gridPanelSize = gridPanelSize;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public JFrame apply(final Model<T> winningModel) {
		final int[] coordDims = winningModel.getCoordinateDimensions();
		final int[] panelDims = Arrays.stream(coordDims).map(dim -> dim * gridPanelSize).toArray();
		final Dimension panelSize = new Dimension(panelDims[0], panelDims[1]);
		final JFrame result = new JFrame("Winning configuration");
		result.setResizable(false);
		final ModelCoordinateGridPanel<?> contentPane = gridPanelFactory.apply(winningModel, panelSize);
		result.setContentPane(contentPane);
		final String desc = "The configuration of the board you need to reach in order to win the game.";
		contentPane.getAccessibleContext().setAccessibleDescription(desc);
		contentPane.setToolTipText(desc);
		return result;
	}

}

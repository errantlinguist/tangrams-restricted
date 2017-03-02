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

import java.awt.Graphics;
import java.awt.LayoutManager;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.swing.JPanel;
import javax.swing.border.Border;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
final class ModelCoordinatePanel<PHBF extends Consumer<? super String> & Supplier<? extends Border>> extends JPanel {

	/**
	 *
	 */
	private static final long serialVersionUID = 8233864105609057151L;

	private final Border defaultBorder;

	private Consumer<? super Graphics> deletegateComponentPainter;

	private final int[] modelCoords;

	private final PHBF playerHighlightBorderFactory;

	private ModelCoordinatePanel(final int[] modelCoords, final LayoutManager layout, final Border defaultBorder,
			final PHBF playerHighlightBorderFactory, final Consumer<? super Graphics> deletegateComponentPainter) {
		super(layout);
		this.modelCoords = modelCoords;
		this.defaultBorder = defaultBorder;
		setBorder(defaultBorder);
		this.playerHighlightBorderFactory = playerHighlightBorderFactory;
		this.deletegateComponentPainter = deletegateComponentPainter;
	}

	ModelCoordinatePanel(final int[] modelCoords, final LayoutManager layout, final Border defaultBorder,
			final PHBF playerHighlightingBorderFactory) {
		this(modelCoords, layout, defaultBorder, playerHighlightingBorderFactory, g -> {
			// Do nothing
		});
	}

	/**
	 * @return the modelCoords
	 */
	public int[] getModelCoords() {
		return modelCoords;
	}

	public void setDelegateComponentPainter(final Consumer<? super Graphics> delegateComponentPainter) {
		deletegateComponentPainter = delegateComponentPainter;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
	 */
	@Override
	protected void paintComponent(final Graphics g) {
		super.paintComponent(g);
		deletegateComponentPainter.accept(g);
	}

	boolean toggleHighlighted(final String playerId) {
		playerHighlightBorderFactory.accept(playerId);
		final Border highlightBorder = playerHighlightBorderFactory.get();
		final boolean result;
		final Border newBorder;
		if (highlightBorder == null) {
			newBorder = defaultBorder;
			result = false;
		} else {
			newBorder = highlightBorder;
			result = true;
		}
		setBorder(newBorder);
		return result;
	}

}
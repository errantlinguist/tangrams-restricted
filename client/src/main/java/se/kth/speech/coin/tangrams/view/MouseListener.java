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

import java.awt.Component;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Arrays;

import javax.swing.JLabel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.game.LocalController;
import se.kth.speech.coin.tangrams.game.LocalController.ValidationStatus;

/**
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
final class MouseListener extends MouseAdapter {

	public interface CallbackHandler {

		boolean confirmMove(int[] sourceCoords, int[] targetCoords);

		Component findComponent(int x, int y);

		void notifyTargetNonAdjacent();

		void notifyTargetOccupied();

		void selectPiece(JLabel selectedPieceLabel);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(MouseListener.class);

	private final CallbackHandler callbackHandler;

	private final LocalController<?> controller;

	private JLabel selectedPieceLabel;

	private ModelCoordinatePanel<?> selectedPieceSource;

	private int xAdjustment;

	private int yAdjustment;

	/**
	 * @param gameBoardPanel
	 */
	MouseListener(final LocalController<?> controller, final CallbackHandler callbackHandler) {
		this.controller = controller;
		this.callbackHandler = callbackHandler;
	}

	@Override
	public void mouseDragged(final MouseEvent me) {
		if (selectedPieceLabel != null) {
			selectedPieceLabel.setLocation(me.getX() + xAdjustment, me.getY() + yAdjustment);
		}
	}

	@Override
	public void mousePressed(final MouseEvent e) {
		selectedPieceLabel = null;
		final Component foundComponent = callbackHandler.findComponent(e.getX(), e.getY());
		if (foundComponent instanceof JLabel) {
			selectedPieceSource = (ModelCoordinatePanel<?>) foundComponent.getParent();

			controller.toggleSelection(selectedPieceSource.getModelCoords());

			final Point parentLocation = selectedPieceSource.getLocation();
			xAdjustment = parentLocation.x - e.getX();
			yAdjustment = parentLocation.y - e.getY();
			selectedPieceLabel = (JLabel) foundComponent;
			selectedPieceLabel.setLocation(e.getX() + xAdjustment, e.getY() + yAdjustment);
			callbackHandler.selectPiece(selectedPieceLabel);
		} else {
			LOGGER.debug("No piece to select.");
		}
	}

	@Override
	public void mouseReleased(final MouseEvent e) {
		if (selectedPieceLabel != null) {
			selectedPieceLabel.setVisible(false);
			final Component foundComponent = callbackHandler.findComponent(e.getX(), e.getY());
			if (foundComponent == null) {
				// The user dropped the panel somewhere on the screen where
				// there is no component; Reset the selection
				resetSelectedPiece();
			} else if (foundComponent instanceof JLabel) {
				callbackHandler.notifyTargetOccupied();
				resetSelectedPiece();
			} else if (foundComponent instanceof ModelCoordinatePanel) {
				// Move the label to the new grid square
				final ModelCoordinatePanel<?> targetGridPanel = (ModelCoordinatePanel<?>) foundComponent;

				final int[] sourceCoords = selectedPieceSource.getModelCoords();
				final int[] targetCoords = targetGridPanel.getModelCoords();
				final ValidationStatus validationStatus = controller.submitMove(sourceCoords, targetCoords);
				switch (validationStatus) {
				case OK: {
					// Add the selected piece's icon to the new panel
					targetGridPanel.add(selectedPieceLabel);
					selectedPieceLabel.setVisible(true);

					if (callbackHandler.confirmMove(sourceCoords, targetCoords)) {
						// Register the move
						LOGGER.debug("Queued move from {} to {}.", Arrays.toString(sourceCoords),
								Arrays.toString(targetCoords));
						controller.endTurn();
						// TODO: Add asynchronous processing here
						// instead of
						// always ending the turn directly afterwards?
					} else {
						// Put the selected piece's icon back into its
						// original grid square
						selectedPieceLabel.setVisible(false);
						targetGridPanel.remove(selectedPieceLabel);
						resetSelectedPiece();
					}
					break;
				}
				case SOURCE_EMPTY:
					throw new AssertionError("The game board panel passed an empty-source move to the controller.");
				case SOURCE_TARGET_SAME:
					resetSelectedPiece();
					break;
				case TARGET_NONADJACENT:
					callbackHandler.notifyTargetNonAdjacent();
					resetSelectedPiece();
					break;
				case TARGET_OCCUPIED:
					throw new AssertionError(
							"The game board panel passed a move to the controller for which the target is already occupied.");
				default:
					throw new AssertionError(String.format("No logic for handling %s.", validationStatus));

				}
			} else {
				// The user dropped the panel onto a component which does not
				// represent any model coordinates; Reset the selection
				resetSelectedPiece();
			}
			controller.toggleSelection(selectedPieceSource.getModelCoords());
		}

	}

	private void resetSelectedPiece() {
		// Put the piece back into its source grid square
		selectedPieceSource.add(selectedPieceLabel);
		selectedPieceLabel.setVisible(true);
	}
}
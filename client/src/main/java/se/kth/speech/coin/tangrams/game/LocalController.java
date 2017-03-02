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
package se.kth.speech.coin.tangrams.game;

import java.util.Arrays;
import java.util.Observable;
import java.util.function.Consumer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.iristk.events.CoordinatePoint;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2016
 *
 */
public final class LocalController<T> extends Observable {

	public enum ValidationStatus {
		OK, SOURCE_EMPTY, SOURCE_TARGET_SAME, TARGET_NONADJACENT, TARGET_OCCUPIED;
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(LocalController.class);

	private boolean isEnabled;

	private final Model<T> model;

	private int moveCount = 0;

	private Move nextTurnMove;

	private final String playerId;

	private final Consumer<? super CoordinatePoint> selectionHook;

	private final Consumer<? super Move> turnCompletionHook;

	public LocalController(final Model<T> model, final String playerId, final boolean isEnabled,
			final Consumer<? super Move> turnCompletionHook, final Consumer<? super CoordinatePoint> selectionHook) {
		this.model = model;
		this.playerId = playerId;
		this.isEnabled = isEnabled;
		this.turnCompletionHook = turnCompletionHook;
		this.selectionHook = selectionHook;
	}

	/**
	 * Calling this method signals that the player using the given
	 * {@link LocalController} instance has ended their turn.
	 */
	public void endTurn() {
		if (!isEnabled) {
			throw new IllegalStateException("Controller not active.");
		}
		if (nextTurnMove == null) {
			throw new IllegalStateException("No move has been submitted.");
		}
		LOGGER.info("Ending turn.");
		{
			final Move move = nextTurnMove;
			final int[] sourceCoords = move.getSource().getCoords();
			final int[] targetCoords = move.getTarget().getCoords();
			LOGGER.info("Moving the piece at {} to {}.", Arrays.toString(sourceCoords), Arrays.toString(targetCoords));
			updateModel(sourceCoords, targetCoords);
			turnCompletionHook.accept(move);
		}
		nextTurnMove = null;
		moveCount++;

	}

	/**
	 * @return the model
	 */
	public Model<T> getModel() {
		return model;
	}

	public int getMoveCount() {
		return moveCount;
	}

	/**
	 * @return the playerId
	 */
	public String getPlayerId() {
		return playerId;
	}

	/**
	 * @return the isEnabled
	 */
	public boolean isEnabled() {
		return isEnabled;
	}

	/**
	 * @param isEnabled
	 *            the isEnabled to set
	 */
	public void setEnabled(final boolean isEnabled) {
		this.isEnabled = isEnabled;
	}

	public ValidationStatus submitMove(final int[] sourceCoords, final int[] targetCoords) {
		if (!isEnabled) {
			throw new IllegalStateException("Controller not active.");
		}

		final ValidationStatus result = validateMove(sourceCoords, targetCoords);
		if (ValidationStatus.OK.equals(result)) {
			nextTurnMove = new Move(new CoordinatePoint(sourceCoords), new CoordinatePoint(targetCoords));
		}
		return result;
	}

	public void toggleSelection(final int[] coords) {
		// FIXME: Make sure that the controller distinguishes between the
		// different users' selections, e.g. if they click the same box, it
		// stays selected rather than being "toggled" off
		LOGGER.debug("Toggling coordinate selection {}.", Arrays.toString(coords));

		final CoordinatePoint coordRecord = new CoordinatePoint(coords);
		final Selection selection = new Selection(playerId, coordRecord);
		// Notify local listeners of (de-)selected coords, e.g. update player's
		// own view and notify the player's own game action module
		setChanged();
		notifyObservers(selection);

		selectionHook.accept(coordRecord);
	}

	private void updateModel(final int[] sourceCoords, final int[] targetCoords) {
		if (model.areCoordinatesOccupied(targetCoords)) {
			throw new IllegalStateException(String.format(
					"Coordinates %s are already occupied; Not a valid move target.", Arrays.toString(targetCoords)));
		}
		final T occupant = model.getCoordinateOccupant(sourceCoords);
		model.setCoordinateOccupant(targetCoords, occupant);
		model.setCoordinateOccupant(sourceCoords, null);
	}

	private ValidationStatus validateMove(final int[] sourceCoords, final int[] targetCoords) {
		final ValidationStatus result;

		if (Arrays.equals(sourceCoords, targetCoords)) {
			result = ValidationStatus.SOURCE_TARGET_SAME;
		} else if (model.areCoordinatesOccupied(sourceCoords)) {
			if (model.areCoordinatesOccupied(targetCoords)) {
				result = ValidationStatus.TARGET_OCCUPIED;
			} else if (model.areCoordinatesAdjacent(sourceCoords, targetCoords)) {
				result = ValidationStatus.OK;
			} else {
				result = ValidationStatus.TARGET_NONADJACENT;
			}
		} else {
			result = ValidationStatus.SOURCE_EMPTY;
		}

		return result;
	}

}

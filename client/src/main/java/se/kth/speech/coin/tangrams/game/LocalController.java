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
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.function.Consumer;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.CoordinatePoint2D;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2016
 *
 */
public final class LocalController<I> extends Observable {

	public enum ValidationStatus {
		OK, SOURCE_EMPTY, SOURCE_TARGET_SAME, TARGET_OCCUPIED;
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(LocalController.class);

	private static Area2D createArea(final SpatialRegion region) {
		return new Area2D(createStartCoords(region), createEndCoords(region));
	}

	private static CoordinatePoint2D createEndCoords(final SpatialRegion region) {
		return new CoordinatePoint2D(region.getXUpperBound(), region.getYUpperBound());
	}

	private static CoordinatePoint2D createStartCoords(final SpatialRegion region) {
		return new CoordinatePoint2D(region.getXLowerBound(), region.getYLowerBound());
	}

	private boolean isEnabled;

	private final SpatialMatrix<I> model;

	private int moveCount = 0;

	private Move nextTurnMove;

	private final String playerId;

	private final Consumer<? super CoordinatePoint2D> selectionHook;

	private final Consumer<? super Move> turnCompletionHook;

	private final Function<Area2D, SpatialRegion> areaSpatialRegionFactory;

	public LocalController(final SpatialMatrix<I> model, final String playerId, final boolean isEnabled,
			final Consumer<? super Move> turnCompletionHook, final Consumer<? super CoordinatePoint2D> selectionHook) {
		this.model = model;
		this.areaSpatialRegionFactory = new AreaSpatialRegionFactory(model);
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
			final SpatialRegion sourceRegion = areaSpatialRegionFactory.apply(move.getSource());
			final SpatialRegion targetRegion = areaSpatialRegionFactory.apply(move.getTarget());
			LOGGER.info("Moving the piece at {} to {}.", sourceRegion, targetRegion);
			updateModel(sourceRegion, targetRegion);
			turnCompletionHook.accept(move);
		}
		nextTurnMove = null;
		moveCount++;

	}

	/**
	 * @return the model
	 */
	public SpatialMatrix<I> getModel() {
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

	public ValidationStatus submitMove(final SpatialRegion sourceRegion, final SpatialRegion targetRegion) {
		if (!isEnabled) {
			throw new IllegalStateException("Controller not active.");
		}

		final ValidationStatus result = validateMove(sourceRegion, targetRegion);
		if (ValidationStatus.OK.equals(result)) {
			nextTurnMove = new Move(createArea(sourceRegion), createArea(targetRegion));
		} else {
			LOGGER.info("Invalid move: {}", result);
		}
		return result;
	}

	public void toggleSelection(final int[] region) {
		// FIXME: Make sure that the controller distinguishes between the
		// different users' selections, e.g. if they click the same box, it
		// stays selected rather than being "toggled" off
		LOGGER.debug("Toggling coordinate selection {}.", Arrays.toString(region));

		final CoordinatePoint2D coordRecord = new CoordinatePoint2D(region);
		final Selection selection = new Selection(playerId, coordRecord);
		// Notify local listeners of (de-)selected region, e.g. update player's
		// own view and notify the player's own game action module
		setChanged();
		notifyObservers(selection);

		selectionHook.accept(coordRecord);
	}

	private void updateModel(final SpatialRegion sourceRegion, final SpatialRegion targetRegion) {
		if (model.isOccupied(targetRegion)) {
			throw new IllegalStateException(
					String.format("%s is already occupied; Not a valid move target.", targetRegion));
		}
		final Iterator<I> occupantIter = model.getElementPlacements().getSubsumedElements(sourceRegion)
				.map(Entry::getValue).iterator();
		final I occupant = occupantIter.next();
		assert !occupantIter.hasNext();
		model.placeElement(occupant, targetRegion);
	}

	private ValidationStatus validateMove(final SpatialRegion sourceRegion, final SpatialRegion targetRegion) {
		final ValidationStatus result;

		if (sourceRegion.equals(targetRegion)) {
			result = ValidationStatus.SOURCE_TARGET_SAME;
		} else if (model.isOccupied(sourceRegion)) {
			if (model.isOccupied(targetRegion)) {
				result = ValidationStatus.TARGET_OCCUPIED;
			} else {
				result = ValidationStatus.OK;
			}
		} else {
			result = ValidationStatus.SOURCE_EMPTY;
		}

		return result;
	}

}

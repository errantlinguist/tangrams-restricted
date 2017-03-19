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
import java.util.function.Predicate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.iristk.events.ActivePlayerChange;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.CoordinatePoint2D;
import se.kth.speech.coin.tangrams.iristk.events.GameEnding;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2016
 *
 */
public final class RemoteController<I> extends Observable {

	private static final Logger LOGGER = LoggerFactory.getLogger(RemoteController.class);

	private final Consumer<ActivePlayerChange> activePlayerChangeHook;

	private final SpatialMatrix<I> model;

	private int moveCount = 0;

	private final Predicate<? super String> playerIdFilter;

	private final Function<Area2D, SpatialRegion> areaSpatialRegionFactory;

	public RemoteController(final SpatialMatrix<I> model, final Consumer<ActivePlayerChange> activePlayerChangeHook,
			final Predicate<? super String> playerIdFilter) {
		this.model = model;
		this.areaSpatialRegionFactory = new AreaSpatialRegionFactory(model);
		this.activePlayerChangeHook = activePlayerChangeHook;
		this.playerIdFilter = playerIdFilter;
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
	 * @param gameEnding
	 */
	public void notifyGameOver(final GameEnding gameEnding) {
		LOGGER.info("The remote controller was notified that the game has ended.");
		// Notify local, lower-level listeners which e.g. update the user's own
		// view
		setChanged();
		notifyObservers(gameEnding);
	}

	/**
	 * @param handoff
	 */
	public void notifyNewActivePlayer(final ActivePlayerChange handoff) {
		LOGGER.debug("The remote controller was notified that player \"{}\" is now active.",
				handoff.getNewActivePlayerId());
		// Notify local, lower-level listeners which e.g. update the user's own
		// view
		setChanged();
		notifyObservers(handoff);
		activePlayerChangeHook.accept(handoff);
	}

	public void notifyPlayerJoined(final PlayerJoinTime playerJoinTime) {
		final String joinedPlayerId = playerJoinTime.getPlayerId();
		LOGGER.debug("The remote controller was notified that \"{}\" has joined the current game.", joinedPlayerId);
		// Notify local, lower-level listeners which e.g. update the user's own
		// view
		setChanged();
		notifyObservers(playerJoinTime);
	}

	public void notifyPlayerSelection(final Selection playerSelection) {
		final String selectingPlayerId = playerSelection.getPlayerId();
		LOGGER.debug("The remote controller was notified that \"{}\" has performed a selection.", selectingPlayerId);
		if (playerIdFilter.test(selectingPlayerId)) {
			final CoordinatePoint2D coords = playerSelection.getCoords();
			LOGGER.debug("Updating coordinate selection {} from \"{}\".", Arrays.toString(coords.getCoords()),
					selectingPlayerId);
			// Notify local, lower-level listeners which e.g. update the user's
			// own
			// view
			setChanged();
			notifyObservers(playerSelection);
		} else {
			LOGGER.debug("Ignoring remote notification about player's own selection.");
		}
	}

	public void notifyPlayerTurn(final Turn turn) {
		final String turnPlayerId = turn.getPlayerId();
		LOGGER.debug("The remote controller was notified that \"{}\" has completed a turn.", turnPlayerId);
		if (playerIdFilter.test(turnPlayerId)) {
			final Move move = turn.getMove();
			final SpatialRegion sourceRegion = areaSpatialRegionFactory.apply(move.getSource());
			final SpatialRegion targetRegion = areaSpatialRegionFactory.apply(move.getTarget());
			LOGGER.info("Moving the piece at {} to {}.", sourceRegion, targetRegion);
			updateModel(sourceRegion, targetRegion);

		} else {
			LOGGER.debug("Skipping model update for remote notification about player's own turn.");
		}
		// Notify local, lower-level listeners which e.g. update the user's
		// own view
		setChanged();
		notifyObservers(turn);
		// Take the greater of the two because it may be possible for two turns
		// to arrive in the wrong order
		moveCount = Math.max(turn.getSequenceNumber(), moveCount);
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

}

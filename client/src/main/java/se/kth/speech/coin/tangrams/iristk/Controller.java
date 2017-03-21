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
package se.kth.speech.coin.tangrams.iristk;

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.game.AreaSpatialRegionFactory;
import se.kth.speech.coin.tangrams.game.PlayerJoinTime;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.CoordinatePoint2D;
import se.kth.speech.coin.tangrams.iristk.events.GameEnding;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.PlayerRoleChange;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2016
 *
 */
public final class Controller {

	public interface Listener {

		void updateGameOver(GameEnding gameEnding);

		void updateNextTurn(Turn turn);

		void updatePlayerRole(PlayerRoleChange change);

		void updatePlayerSelection(Selection selection);

		void updateSelectionAccepted(Selection selection);

		void updateSelectionRejected(Selection selection);

		void updateTurnCompletion(Turn turn);

	}

	public enum ValidationStatus {
		OK, SOURCE_EMPTY, SOURCE_TARGET_SAME, TARGET_OCCUPIED;
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(Controller.class);

	private static Area2D createArea(final SpatialRegion region) {
		return new Area2D(createStartCoords(region), createEndCoords(region));
	}

	private static CoordinatePoint2D createEndCoords(final SpatialRegion region) {
		return new CoordinatePoint2D(region.getXUpperBound(), region.getYUpperBound());
	}

	private static CoordinatePoint2D createStartCoords(final SpatialRegion region) {
		return new CoordinatePoint2D(region.getXLowerBound(), region.getYLowerBound());
	}

	private final Function<Area2D, SpatialRegion> areaSpatialRegionFactory;

	private final Set<Listener> listeners;

	private final GameManagementClientModule clientModule;

	private final SpatialMatrix<Integer> model;

	private int moveCount = 0;

	private Move nextTurnMove;

	private final String playerId;

	private PlayerRole role;

	private Entry<Integer, Area2D> selectedPiece;

	public Controller(final SpatialMatrix<Integer> model, final String playerId, final PlayerRole role,
			final GameManagementClientModule clientModule) {
		this.model = model;
		this.playerId = playerId;
		this.role = role;
		this.clientModule = clientModule;

		areaSpatialRegionFactory = new AreaSpatialRegionFactory(model);
		listeners = Collections.newSetFromMap(new IdentityHashMap<>());
	}

	public void confirmSelection() {
		final PlayerRole requiredRole = PlayerRole.WAITING_FOR_SELECTION;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Role is currently not %s but rather %s.", requiredRole, role));
		}
		clientModule.requestTurnCompletion(nextTurnMove);
		moveCount++;
		role = PlayerRole.SELECTING;
	}

	/**
	 * @return the listeners
	 */
	public Set<Listener> getListeners() {
		return listeners;
	}

	/**
	 * @return the model
	 */
	public SpatialMatrix<Integer> getModel() {
		return model;
	}

	public int getMoveCount() {
		return moveCount;
	}

	/**
	 * @return the nextTurnMove
	 */
	public Move getNextTurnMove() {
		return nextTurnMove;
	}

	/**
	 * @return the playerId
	 */
	public String getPlayerId() {
		return playerId;
	}

	/**
	 * @return the role
	 */
	public PlayerRole getRole() {
		return role;
	}

	/**
	 * @return the selectedPiece
	 */
	public Entry<Integer, Area2D> getSelectedPiece() {
		return selectedPiece;
	}

	public boolean isSelectionCorrect() {
		return nextTurnMove.getPieceId().equals(selectedPiece.getKey());
	}

	/**
	 * @param gameEnding
	 */
	public void notifyGameOver(final GameEnding gameEnding) {
		LOGGER.info("The controller was notified that the game has ended.");
		listeners.forEach(listener -> listener.updateGameOver(gameEnding));
		// Notify local, lower-level listeners which e.g. update the user's own
		// view
		// setChanged();
		// notifyObservers(gameEnding);
	}

	public void notifyNextTurn(final Turn turn) {
		final String submittingPlayerId = turn.getPlayerId();
		LOGGER.debug("The controller was notified that \"{}\" has submitted a turn.", submittingPlayerId);
		// if (playerIdFilter.test(turnPlayerId)) {
		nextTurnMove = turn.getMove();
		if (playerId.equals(submittingPlayerId)) {
			role = PlayerRole.WAITING_FOR_SELECTION;
		} else {
			role = PlayerRole.SELECTING;
		}
		final PlayerRoleChange roleChange = new PlayerRoleChange(submittingPlayerId, role);
		updatePlayerRole(roleChange);
		listeners.forEach(listener -> listener.updateNextTurn(turn));
		// final Move move = turn.getMove();
		// final SpatialRegion sourceRegion =
		// areaSpatialRegionFactory.apply(move.getSource());
		// final SpatialRegion targetRegion =
		// areaSpatialRegionFactory.apply(move.getTarget());
		// LOGGER.info("Moving the piece at {} to {}.", sourceRegion,
		// targetRegion);
		// updateModel(sourceRegion, targetRegion);

		// } else {
		// LOGGER.debug("Skipping model update for remote notification about
		// player's own turn.");
		// }
		// Notify local, lower-level listeners which e.g. update the user's
		// own view
		// setChanged();
		// notifyObservers(turn);
		// Take the greater of the two because it may be possible for two turns
		// to arrive in the wrong order
		moveCount = Math.max(turn.getSequenceNumber(), moveCount);
	}

	public void notifyPlayerJoined(final PlayerJoinTime playerJoinTime) {
		final String joinedPlayerId = playerJoinTime.getPlayerId();
		LOGGER.debug("The controller was notified that \"{}\" has joined the current game.", joinedPlayerId);
		// Notify local, lower-level listeners which e.g. update the user's own
		// view
		// setChanged();
		// notifyObservers(playerJoinTime);
	}

	public void notifyPlayerSelection(final Selection selection) {
		final String selectingPlayerId = selection.getPlayerId();
		LOGGER.debug("The controller was notified that \"{}\" has performed a selection.", selectingPlayerId);
		// if (playerIdFilter.test(selectingPlayerId)) {
		listeners.forEach(listener -> listener.updatePlayerSelection(selection));
		final Integer pieceId = selection.getPieceId();
		LOGGER.debug("Updating piece selection {} from \"{}\".", pieceId, selectingPlayerId);
		// Notify local, lower-level listeners which e.g. update the user's
		// own
		// view
		// setChanged();
		// notifyObservers(playerSelection);
		// } else {
		// LOGGER.debug("Ignoring remote notification about player's own
		// selection.");
		// }
		final String submittingPlayerId = selection.getPlayerId();
		LOGGER.debug("Controller was notified of a selection, submitted by \"{}\".", submittingPlayerId);
		selectedPiece = new MutablePair<>(selection.getPieceId(), selection.getArea());
		// setChanged();
		// notifyObservers(selection);
		if (playerId.equals(submittingPlayerId)) {
			role = PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION;
		} else {
			role = PlayerRole.TURN_SUBMISSION;
		}
		final PlayerRoleChange roleChange = new PlayerRoleChange(submittingPlayerId, role);
		updatePlayerRole(roleChange);
	}

	public void notifySelectionRejected(final Selection playerSelection) {
		final String rejectingPlayerId = playerSelection.getPlayerId();
		LOGGER.debug("The controller was notified that \"{}\" has rejected a selection.", rejectingPlayerId);
		listeners.forEach(listener -> listener.updateSelectionRejected(playerSelection));
		// if (playerIdFilter.test(rejectingPlayerId)) {
		final Integer pieceId = playerSelection.getPieceId();
		LOGGER.debug("Updating piece selection {} from \"{}\".", pieceId, rejectingPlayerId);
		// Notify local, lower-level listeners which e.g. update the user's
		// own
		// view
		// setChanged();
		// notifyObservers(playerSelection);
		// } else {
		// LOGGER.debug("Procesing remote notification about player's own
		// selection.");
		// }
		LOGGER.debug("The local controller was notified that \"{}\" has rejected a selection.", rejectingPlayerId);
		if (playerId.equals(rejectingPlayerId)) {
			role = PlayerRole.WAITING_FOR_SELECTION;
		} else {
			role = PlayerRole.SELECTING;
		}
		final PlayerRoleChange roleChange = new PlayerRoleChange(rejectingPlayerId, role);
		updatePlayerRole(roleChange);
	}

	public void notifyTurnComplete(final Turn turn) {
		final String submittingPlayerId = turn.getPlayerId();
		LOGGER.debug("The controller was notified that \"{}\" has completed a turn.", submittingPlayerId);
		// if (playerIdFilter.test(turnPlayerId)) {
		nextTurnMove = null;
		if (playerId.equals(submittingPlayerId)) {
			role = PlayerRole.SELECTING;
		} else {
			role = PlayerRole.TURN_SUBMISSION;
		}
		final PlayerRoleChange roleChange = new PlayerRoleChange(submittingPlayerId, role);
		updatePlayerRole(roleChange);
		listeners.forEach(listener -> listener.updateTurnCompletion(turn));
		// final Move move = turn.getMove();
		// final SpatialRegion sourceRegion =
		// areaSpatialRegionFactory.apply(move.getSource());
		// final SpatialRegion targetRegion =
		// areaSpatialRegionFactory.apply(move.getTarget());
		// LOGGER.info("Moving the piece at {} to {}.", sourceRegion,
		// targetRegion);
		// updateModel(sourceRegion, targetRegion);

		// } else {
		// LOGGER.debug("Skipping model update for remote notification about
		// player's own turn.");
		// }
		// Notify local, lower-level listeners which e.g. update the user's
		// own view
		// setChanged();
		// notifyObservers(turn);
		// Take the greater of the two because it may be possible for two turns
		// to arrive in the wrong order
		moveCount = Math.max(turn.getSequenceNumber(), moveCount);

	}

	public void rejectSelection() {
		final PlayerRole requiredRole = PlayerRole.WAITING_FOR_SELECTION;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Role is currently not %s but rather %s.", requiredRole, role));
		}
		clientModule.rejectSelection(selectedPiece.getKey(), selectedPiece.getValue());
	}

	public ValidationStatus submitNextMove(final SpatialRegion sourceRegion, final SpatialRegion targetRegion,
			final Integer pieceId) {
		final PlayerRole requiredRole = PlayerRole.TURN_SUBMISSION;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Role is currently not %s but rather %s.", requiredRole, role));
		}

		final ValidationStatus result = validateMove(sourceRegion, targetRegion);
		if (ValidationStatus.OK.equals(result)) {
			nextTurnMove = new Move(createArea(sourceRegion), createArea(targetRegion), pieceId);
			// Notify local listeners of (de-)selected region, e.g. update
			// player's
			// own view and notify the player's own game action module
			// setChanged();
			// notifyObservers(nextTurnMove);
			clientModule.requestNextTurn(nextTurnMove);
			role = PlayerRole.WAITING_FOR_SELECTION;
			final PlayerRoleChange roleChange = new PlayerRoleChange(playerId, role);
			updatePlayerRole(roleChange);
		} else {
			throw new IllegalArgumentException("Invalid move: " + result);
		}
		return result;
	}

	/**
	 * @param key
	 */
	public void submitSelection(final Entry<Integer, SpatialRegion> pieceRegion) {
		final PlayerRole requiredRole = PlayerRole.SELECTING;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Role is currently not %s but rather %s.", requiredRole, role));
		}
		selectedPiece = new MutablePair<>(pieceRegion.getKey(), createArea(pieceRegion.getValue()));
		clientModule.requestUserSelection(selectedPiece.getKey(), selectedPiece.getValue());
		role = PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION;
		// if (selectedPiece == null) {
		// throw new IllegalStateException("No selection has been submitted.");
		// }
		// LOGGER.info("Submitting selection.");
		// {
		// final Move move = nextTurnMove;
		// final SpatialRegion sourceRegion =
		// areaSpatialRegionFactory.apply(move.getSource());
		// final SpatialRegion targetRegion =
		// areaSpatialRegionFactory.apply(move.getTarget());
		// LOGGER.info("Moving the piece at {} to {}.", sourceRegion,
		// targetRegion);
		// // updateModel(sourceRegion, targetRegion);
		// nextMoveSubmissionHook.accept(move);
		// }
		// moveCount++;
	}

	public void updatePlayerRole(final PlayerRoleChange roleChange) {
		// if (roleChange.getPlayerId().equals(playerId)) {
		role = roleChange.getRole();
		listeners.forEach(listener -> listener.updatePlayerRole(roleChange));
		// setChanged();
		// notifyObservers(roleChange);
		// }
	}

	// private void updateModel(final SpatialRegion sourceRegion, final
	// SpatialRegion targetRegion) {
	// if (model.isOccupied(targetRegion)) {
	// throw new IllegalStateException(
	// String.format("%s is already occupied; Not a valid move target.",
	// targetRegion));
	// }
	// final Iterator<Integer> occupantIter =
	// model.getElementPlacements().getSubsumedElements(sourceRegion)
	// .map(Entry::getValue).iterator();
	// final Integer occupant = occupantIter.next();
	// assert !occupantIter.hasNext();
	// model.placeElement(occupant, targetRegion);
	// }

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

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

import java.util.Map.Entry;
import java.util.Observable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.RemoteController;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.CoordinatePoint2D;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.PlayerRoleChange;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2016
 *
 */
public final class LocalController extends Observable implements RemoteController.Listener {

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

	private final GameManagementClientModule clientModule;

	private final SpatialMatrix<Integer> model;

	private int moveCount = 0;

	private Move nextTurnMove;

	private final String playerId;

	private PlayerRole role;

	private Entry<Integer, Area2D> selectedPiece;

	public LocalController(final SpatialMatrix<Integer> model, final String playerId, final PlayerRole role,
			final GameManagementClientModule clientModule) {
		this.model = model;
		this.playerId = playerId;
		this.role = role;
		this.clientModule = clientModule;
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
			setChanged();
			notifyObservers(nextTurnMove);
			clientModule.requestNextTurn(nextTurnMove);
			role = PlayerRole.WAITING_FOR_SELECTION;
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
		this.selectedPiece = new MutablePair<>(pieceRegion.getKey(), createArea(pieceRegion.getValue()));
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

	@Override
	public void updateNextTurn(final Turn turn) {
		final String submittingPlayerId = turn.getPlayerId();
		LOGGER.debug("Local controller was notified of the next move, submitted by \"{}\".", submittingPlayerId);
		nextTurnMove = turn.getMove();
		// Notify local, lower-level listeners which e.g. update the user's
		// own view
//		setChanged();
//		notifyObservers(turn);
		if (playerId.equals(submittingPlayerId)) {
			role = PlayerRole.WAITING_FOR_SELECTION;
		} else {
			role = PlayerRole.SELECTING;
		}
		PlayerRoleChange roleChange = new PlayerRoleChange(submittingPlayerId, role);
		updatePlayerRole(roleChange);
	}

	@Override
	public void updatePlayerRole(final PlayerRoleChange roleChange) {
		if (roleChange.getPlayerId().equals(playerId)) {
			role = roleChange.getRole();
			setChanged();
			notifyObservers(roleChange);
		}
	}

	@Override
	public void updatePlayerSelection(final Selection selection) {
		final String submittingPlayerId = selection.getPlayerId();
		LOGGER.debug("Local controller was notified of a selection, submitted by \"{}\".", submittingPlayerId);
		selectedPiece = new MutablePair<>(selection.getPieceId(), selection.getArea());
		setChanged();
		notifyObservers(selection);
		if (playerId.equals(submittingPlayerId)) {
			role = PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION;
		} else {
			role = PlayerRole.TURN_SUBMISSION;
		}
		PlayerRoleChange roleChange = new PlayerRoleChange(submittingPlayerId, role);
		updatePlayerRole(roleChange);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see se.kth.speech.coin.tangrams.game.RemoteController.Listener#
	 * updateSelectionRejected(se.kth.speech.coin.tangrams.iristk.events.
	 * Selection)
	 */
	@Override
	public void updateSelectionRejected(Selection selection) {
		final String rejectingPlayerId = selection.getPlayerId();
		LOGGER.debug("The local controller was notified that \"{}\" has rejected a selection.", rejectingPlayerId);
		if (playerId.equals(rejectingPlayerId)) {
			role = PlayerRole.WAITING_FOR_SELECTION;
		} else {
			role = PlayerRole.SELECTING;
		}
		PlayerRoleChange roleChange = new PlayerRoleChange(rejectingPlayerId, role);
		updatePlayerRole(roleChange);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see se.kth.speech.coin.tangrams.game.RemoteController.Listener#
	 * updateSelectionAccepted(se.kth.speech.coin.tangrams.iristk.events.
	 * Selection)
	 */
	@Override
	public void updateSelectionAccepted(Selection selection) {
		final String acceptingPlayerId = selection.getPlayerId();
		LOGGER.debug("The local controller was notified that \"{}\" has accepted a selection.", acceptingPlayerId);
		if (playerId.equals(acceptingPlayerId)) {
			role = PlayerRole.WAITING_FOR_SELECTION;
		} else {
			role = PlayerRole.SELECTING;
		}
		PlayerRoleChange roleChange = new PlayerRoleChange(acceptingPlayerId, role);
		updatePlayerRole(roleChange);

	}

	@Override
	public void updateTurnCompletion(final Turn turn) {
		final String submittingPlayerId = turn.getPlayerId();
		LOGGER.debug("Local controller was notified of a turn completion, submitted by \"{}\".", submittingPlayerId);
		nextTurnMove = null;
		setChanged();
		notifyObservers(turn);
		if (playerId.equals(submittingPlayerId)) {
			role = PlayerRole.SELECTING;
		} else {
			role = PlayerRole.TURN_SUBMISSION;
		}
		PlayerRoleChange roleChange = new PlayerRoleChange(submittingPlayerId, role);
		updatePlayerRole(roleChange);
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

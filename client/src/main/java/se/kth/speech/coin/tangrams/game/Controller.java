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

import java.util.Collection;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Map.Entry;
import java.util.Set;

import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMap;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.AreaSpatialRegionFactory;
import se.kth.speech.coin.tangrams.iristk.GameManagementClientModule;
import se.kth.speech.coin.tangrams.iristk.MyLogger;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.CoordinatePoint2D;
import se.kth.speech.coin.tangrams.iristk.events.GameEnding;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Nov 2016
 *
 */
public final class Controller {

	public interface Listener {

		void updateGameOver(GameEnding gameEnding);

		void updateNextMove(Move move);

		void updatePieceMoved(SpatialRegion source, SpatialRegion target);

		void updatePlayerJoined(String joinedPlayerId, long time);

		void updatePlayerRole(PlayerRole newRole);

		void updatePlayerSelection(Selection selection);

		void updateSelectionRejected(Selection selection);

		void updateTurnCompleted(Turn turn);

		/**
		 * A hook for updating listeners for a new turn count, i.e. the sequence
		 * number of the next turn to be completed.
		 *
		 * @param newCount
		 *            The new turn count.
		 */
		void updateTurnCount(int newCount);

	}

	private enum ValidationStatus {
		OK, SOURCE_EMPTY, SOURCE_TARGET_SAME, TARGET_OCCUPIED;
	}

	// private static final Logger LOGGER =
	// LoggerFactory.getLogger(Controller.class);
	private static final MyLogger LOGGER = new MyLogger();

	private static Area2D createArea(final SpatialRegion region) {
		return new Area2D(createStartCoords(region), createEndCoords(region));
	}

	private static CoordinatePoint2D createEndCoords(final SpatialRegion region) {
		return new CoordinatePoint2D(region.getXUpperBound(), region.getYUpperBound());
	}

	private static CoordinatePoint2D createStartCoords(final SpatialRegion region) {
		return new CoordinatePoint2D(region.getXLowerBound(), region.getYLowerBound());
	}

	private transient final AreaSpatialRegionFactory areaRegionFactory;

	private final GameManagementClientModule clientModule;

	private final Set<Listener> listeners;

	private final SpatialMatrix<Integer> model;

	private int turnCount = 0;

	private Move nextMove;

	private final String playerId;

	private PlayerRole role;

	private Entry<Integer, SpatialRegion> selectedPiece;

	public Controller(final SpatialMatrix<Integer> model, final String playerId, final PlayerRole role,
			final GameManagementClientModule clientModule) {
		this.model = model;
		areaRegionFactory = new AreaSpatialRegionFactory(model);
		this.playerId = playerId;
		this.role = role;
		this.clientModule = clientModule;

		listeners = Collections.newSetFromMap(new IdentityHashMap<>());
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

	public int getTurnCount() {
		return turnCount;
	}

	public boolean isSelectionCorrect() {
		return nextMove.getPieceId().equals(selectedPiece.getKey());
	}

	/**
	 * @param gameEnding
	 */
	public void notifyGameOver(final GameEnding gameEnding) {
		LOGGER.info("The controller was notified that the game has ended.");
		listeners.forEach(listener -> listener.updateGameOver(gameEnding));
	}

	public void notifyNextMove(final String submittingPlayerId, final Move move) {
		LOGGER.debug("The controller was notified that \"{}\" has submitted a new move.", submittingPlayerId);
		final PlayerRole requiredRole = PlayerRole.WAITING_FOR_NEXT_MOVE;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Wrong role for notifying selection: Should be %s but is %s.", requiredRole, role));
		}

		nextMove = move;
		listeners.forEach(listener -> listener.updateNextMove(nextMove));

		updatePlayerRole(PlayerRole.SELECTING);
	}

	public void notifyPlayerJoined(final String joinedPlayerId, final long time) {
		LOGGER.debug("The controller was notified that \"{}\" has joined the current game.", joinedPlayerId);
		listeners.forEach(listener -> listener.updatePlayerJoined(joinedPlayerId, time));
	}

	public void notifyPlayerSelection(final String selectingPlayerId, final Selection selection) {
		LOGGER.debug("The controller was notified that \"{}\" has performed a selection.", selectingPlayerId);
		final PlayerRole requiredRole = PlayerRole.WAITING_FOR_SELECTION;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Wrong role for notifying selection: Should be %s but is %s.", requiredRole, role));
		}

		final Integer pieceId = selection.getPieceId();
		LOGGER.debug("Updating selection of piece \"{}\" from \"{}\".", pieceId, selectingPlayerId);
		selectedPiece = new MutablePair<>(selection.getPieceId(), areaRegionFactory.apply(selection.getArea()));

		// NOTE: The update methods for the listeners call methods on this
		// instance, so the role needs to be changed before calling the listener
		// update methods
		updatePlayerRole(PlayerRole.SELECTION_CONFIRMATION);

		listeners.forEach(listener -> listener.updatePlayerSelection(selection));

	}

	public void notifySelectionRejected(final String rejectingPlayerId, final Selection selection) {
		LOGGER.debug("The controller was notified that \"{}\" has rejected a selection.", rejectingPlayerId);
		final PlayerRole requiredRole = PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Wrong role for notifying selection: Should be %s but is %s.", requiredRole, role));
		}

		listeners.forEach(listener -> listener.updateSelectionRejected(selection));

		// Go back to selecting a new piece
		updatePlayerRole(PlayerRole.SELECTING);
	}

	public void notifyTurnComplete(final String submittingPlayerId, final Move move) {
		LOGGER.debug("The controller was notified that \"{}\" has completed a turn.", submittingPlayerId);
		final PlayerRole requiredRole = PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Wrong role for notifying selection: Should be %s but is %s.", requiredRole, role));
		}

		updatePiecePositions(move);

		LOGGER.debug("Old turn count was {}.", turnCount);
		final Turn turn = new Turn(submittingPlayerId, move, turnCount);
		listeners.forEach(listener -> listener.updateTurnCompleted(turn));
		nextMove = null;
		turnCount++;
		LOGGER.debug("New turn count is {}.", turnCount);
		// Update listeners for current turn count (i.e. the sequence number of
		// the next turn to be completed)
		listeners.forEach(listener -> listener.updateTurnCount(turnCount));

		// Now it's this player's turn to submit a move
		updatePlayerRole(PlayerRole.MOVE_SUBMISSION);
	}

	public void submitNextMove(final SpatialRegion sourceRegion, final SpatialRegion targetRegion,
			final Integer pieceId) {
		final PlayerRole requiredRole = PlayerRole.MOVE_SUBMISSION;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Role is currently not %s but rather %s.", requiredRole, role));
		}

		final ValidationStatus validationStatus = validateMove(sourceRegion, targetRegion);
		if (!ValidationStatus.OK.equals(validationStatus)) {
			throw new IllegalArgumentException("Invalid move: " + validationStatus);
		}
		nextMove = new Move(createArea(sourceRegion), createArea(targetRegion), pieceId);
		clientModule.requestNextMove(nextMove);

		updatePlayerRole(PlayerRole.WAITING_FOR_SELECTION);
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
		selectedPiece = pieceRegion;
		clientModule.requestSelection(selectedPiece.getKey(), createArea(selectedPiece.getValue()));

		updatePlayerRole(PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION);
	}

	public void submitSelectionRejection() {
		final PlayerRole requiredRole = PlayerRole.SELECTION_CONFIRMATION;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Role is currently not %s but rather %s.", requiredRole, role));
		}
		clientModule.rejectSelection(selectedPiece.getKey(), createArea(selectedPiece.getValue()));
		updatePlayerRole(PlayerRole.WAITING_FOR_SELECTION);
	}

	public void submitTurnComplete() {
		final PlayerRole requiredRole = PlayerRole.SELECTION_CONFIRMATION;
		if (!requiredRole.equals(role)) {
			throw new IllegalStateException(
					String.format("Role is currently not %s but rather %s.", requiredRole, role));
		}
		clientModule.requestTurnCompletion(nextMove);
		updatePiecePositions(nextMove);
		nextMove = null;
		selectedPiece = null;

		LOGGER.debug("Old turn count was {}.", turnCount);
		turnCount++;
		LOGGER.debug("New turn is {}.", turnCount);
		// Update listeners for current turn count (i.e. the sequence number of
		// the next turn to be completed)
		listeners.forEach(listener -> listener.updateTurnCount(turnCount));

		// Now it's this player's turn to wait for the other player to submit a
		// move
		updatePlayerRole(PlayerRole.WAITING_FOR_NEXT_MOVE);
	}

	private void updatePiecePositions(final Move move) {
		final SpatialRegion source = areaRegionFactory.apply(move.getSource());
		final SpatialRegion target = areaRegionFactory.apply(move.getTarget());
		updatePiecePositions(source, target);
	}

	private void updatePiecePositions(final SpatialRegion source, final SpatialRegion target) {
		final SpatialMap<Integer> piecePlacements = model.getElementPlacements();
		final Collection<Integer> pieceIds = piecePlacements.getMinimalRegionElements().get(source);
		for (final Integer pieceId : pieceIds) {
			model.placeElement(pieceId, target);
		}
		model.clearRegion(source);
		listeners.forEach(listener -> listener.updatePieceMoved(source, target));
	}

	private void updatePlayerRole(final PlayerRole newRole) {
		role = newRole;
		listeners.forEach(listener -> listener.updatePlayerRole(newRole));
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

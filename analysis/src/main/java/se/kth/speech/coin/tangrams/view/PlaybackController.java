/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialMatrixRegionElementMover;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.AreaSpatialRegionFactory;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.GameplayController;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.SpatialRegionAreaFactory;
import se.kth.speech.coin.tangrams.game.Turn;
import se.kth.speech.coin.tangrams.iristk.GameManagementClient;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

final class PlaybackController implements Controller {

	private static final int BAD_TURN_SCORE_DIFF = -2;

	private static final int GOOD_TURN_SCORE_DIFF = 1;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameplayController.class);

	private static final Function<SpatialRegion, Area2D> REGION_AREA_FACTORY = new SpatialRegionAreaFactory();

	private final AreaSpatialRegionFactory areaRegionFactory;

	private final GameManagementClient clientModule;

	private final Set<Listener> listeners;

	private final SpatialMatrix<Integer> model;

	private Move nextMove;

	private final SpatialMatrixRegionElementMover<Integer> piecePosUpdater;

	private int score;

	private Entry<Integer, SpatialRegion> selectedPiece;

	private int turnCount;

	public PlaybackController(final SpatialMatrix<Integer> model, final GameManagementClient clientModule) {
		this.model = model;
		areaRegionFactory = new AreaSpatialRegionFactory(model);
		piecePosUpdater = new SpatialMatrixRegionElementMover<>(model);
		this.clientModule = clientModule;

		listeners = Collections.newSetFromMap(new IdentityHashMap<>());
	}

	/**
	 * @return the listeners
	 */
	@Override
	public Set<Listener> getListeners() {
		return listeners;
	}

	/**
	 * @return the model
	 */
	@Override
	public SpatialMatrix<Integer> getModel() {
		return model;
	}

	/**
	 * @return the score
	 */
	@Override
	public int getScore() {
		return score;
	}

	@Override
	public int getTurnCount() {
		return turnCount;
	}

	@Override
	public boolean isSelectionCorrect() {
		return nextMove.getPieceId().equals(selectedPiece.getKey());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller#notifyNextMove(java.lang.
	 * String, se.kth.speech.coin.tangrams.iristk.events.Move)
	 */
	@Override
	public void notifyNextMove(final String submittingPlayerId, final Move move) {
		LOGGER.debug("The controller was notified that \"{}\" has submitted a new move.", submittingPlayerId);
		nextMove = move;
		listeners.forEach(listener -> listener.updateNextMove(nextMove));

		updatePlayerRole(PlayerRole.SELECTING);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller#notifyPlayerJoined(java.lang.
	 * String, long)
	 */
	@Override
	public void notifyPlayerJoined(final String joinedPlayerId, final long time) {
		LOGGER.debug("The controller was notified that \"{}\" has joined the current game.", joinedPlayerId);
		listeners.forEach(listener -> listener.updatePlayerJoined(joinedPlayerId, time));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller#notifyPlayerSelection(java.
	 * lang.String, se.kth.speech.coin.tangrams.iristk.events.Selection)
	 */
	@Override
	public void notifyPlayerSelection(final String selectingPlayerId, final Selection selection) {
		LOGGER.debug("The controller was notified that \"{}\" has performed a selection.", selectingPlayerId);
		final Integer pieceId = selection.getPieceId();
		LOGGER.debug("Updating selection of piece \"{}\" from \"{}\".", pieceId, selectingPlayerId);
		selectedPiece = new MutablePair<>(selection.getPieceId(), areaRegionFactory.apply(selection.getArea()));

		// NOTE: The update methods for the listeners call methods on this
		// instance, so the role needs to be changed before calling the listener
		// update methods
		updatePlayerRole(PlayerRole.SELECTION_CONFIRMATION);

		listeners.forEach(
				listener -> listener.updatePlayerSelection(pieceId, areaRegionFactory.apply(selection.getArea())));

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller#notifySelectionRejected(java.
	 * lang.String, se.kth.speech.coin.tangrams.iristk.events.Selection)
	 */
	@Override
	public void notifySelectionRejected(final String rejectingPlayerId, final Selection selection) {
		LOGGER.debug("The controller was notified that \"{}\" has rejected a selection.", rejectingPlayerId);
		listeners.forEach(listener -> listener.updateSelectionRejected(selection.getPieceId(),
				areaRegionFactory.apply(selection.getArea())));

		updateScore(BAD_TURN_SCORE_DIFF);

		// Go back to selecting a new piece
		updatePlayerRole(PlayerRole.SELECTING);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller#notifyTurnComplete(java.lang.
	 * String, se.kth.speech.coin.tangrams.iristk.events.Move)
	 */
	@Override
	public void notifyTurnComplete(final String submittingPlayerId, final Move move) {
		LOGGER.debug("The controller was notified that \"{}\" has completed a turn.", submittingPlayerId);
		updatePiecePositions(move);

		final Entry<SpatialRegion, SpatialRegion> regionMove = createRegionMovePair(move);
		final Turn turn = new Turn(submittingPlayerId, regionMove, turnCount);
		listeners.forEach(listener -> listener.updateTurnCompleted(turn));
		nextMove = null;

		incrementTurnCount();
		updateScore(GOOD_TURN_SCORE_DIFF);

		// Now it's this player's turn to submit a move
		updatePlayerRole(PlayerRole.MOVE_SUBMISSION);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller#submitNextMove(se.kth.speech.
	 * SpatialRegion, se.kth.speech.SpatialRegion, java.lang.Integer)
	 */
	@Override
	public void submitNextMove(final SpatialRegion sourceRegion, final SpatialRegion targetRegion,
			final Integer pieceId) {
		nextMove = new Move(REGION_AREA_FACTORY.apply(sourceRegion), REGION_AREA_FACTORY.apply(targetRegion), pieceId);
		clientModule.requestNextMove(nextMove);

		updatePlayerRole(PlayerRole.WAITING_FOR_SELECTION);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller#submitSelection(java.util.Map
	 * .Entry)
	 */
	@Override
	public void submitSelection(final Entry<Integer, SpatialRegion> pieceRegion) {
		selectedPiece = pieceRegion;
		clientModule.requestSelection(selectedPiece.getKey(), REGION_AREA_FACTORY.apply(selectedPiece.getValue()));

		updatePlayerRole(PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller#submitSelectionRejection()
	 */
	@Override
	public void submitSelectionRejection() {
		clientModule.rejectSelection(selectedPiece.getKey(), REGION_AREA_FACTORY.apply(selectedPiece.getValue()));

		updateScore(BAD_TURN_SCORE_DIFF);

		updatePlayerRole(PlayerRole.WAITING_FOR_SELECTION);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.game.Controller#submitTurnComplete()
	 */
	@Override
	public void submitTurnComplete() {
		clientModule.requestTurnCompletion(nextMove);
		updatePiecePositions(nextMove);
		nextMove = null;
		selectedPiece = null;

		incrementTurnCount();
		updateScore(GOOD_TURN_SCORE_DIFF);

		// Now it's this player's turn to wait for the other player to submit a
		// move
		updatePlayerRole(PlayerRole.WAITING_FOR_NEXT_MOVE);
	}

	private Entry<SpatialRegion, SpatialRegion> createRegionMovePair(final Move move) {
		final SpatialRegion source = areaRegionFactory.apply(move.getSource());
		final SpatialRegion target = areaRegionFactory.apply(move.getTarget());
		return new MutablePair<>(source, target);
	}

	private void incrementTurnCount() {
		LOGGER.debug("Old turn count was {}.", turnCount);
		// NOTE: increment here, OUTSIDE of the listener notification loop!
		turnCount++;
		LOGGER.debug("New turn count is {}.", turnCount);
		// Update listeners for current turn count (i.e. the sequence number of
		// the next turn to be completed)
		listeners.forEach(listener -> listener.updateTurnCount(turnCount));
	}

	private void updatePiecePositions(final Move move) {
		final SpatialRegion source = areaRegionFactory.apply(move.getSource());
		final SpatialRegion target = areaRegionFactory.apply(move.getTarget());
		piecePosUpdater.accept(source, target);
	}

	private void updatePlayerRole(final PlayerRole newRole) {
		listeners.forEach(listener -> listener.updatePlayerRole(newRole));
	}

	private void updateScore(final int diff) {
		LOGGER.debug("Old score was {}.", score);
		// NOTE: set here, OUTSIDE of the listener notification loop!
		score += diff;
		LOGGER.debug("New score is {}.", score);
		// Update listeners for current score
		listeners.forEach(listener -> listener.updateScore(score));
	}

}
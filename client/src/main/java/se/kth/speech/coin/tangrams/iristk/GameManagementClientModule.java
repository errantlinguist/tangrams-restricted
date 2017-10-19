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

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Random;
import java.util.function.Consumer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.BiMap;

import iristk.system.Event;
import iristk.system.IrisModule;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.GameplayController;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.GameModelMatrixUnmarshaller;
import se.kth.speech.coin.tangrams.iristk.events.GameStateDescription;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;
import se.kth.speech.coin.tangrams.iristk.events.ModelDescription;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

public final class GameManagementClientModule extends IrisModule implements GameManagementClient {

	private static final GameModelMatrixUnmarshaller GAME_MODEL_MATRIX_UNMARSHALLER = new GameModelMatrixUnmarshaller(
			SpatialMatrix.Factory.STABLE_ITER_ORDER);

	private static final Logger LOGGER = LoggerFactory.getLogger(GameManagementClientModule.class);

	private Controller controller;

	private final String gameId;

	/**
	 * <strong>NOTE:</strong> It is better to pass the new game-handling logic
	 * as a separate object so that this class needn't be abstract, which means
	 * that it has a concrete name which is sent along with its respective
	 * broker messages (anonymous classes have no name assigned to the events
	 * they send).
	 */
	private final Consumer<? super GameState> newGameHandler;

	private final String playerId;

	public GameManagementClientModule(final String gameId, final String playerId,
			final Consumer<? super GameState> newGameHandler) {
		this.gameId = gameId;
		this.playerId = playerId;
		this.newGameHandler = newGameHandler;
	}

	@Override
	public void init() {
		subscribe(GameManagementEvent.EVENT_NAME_QUALIFIER + "**");
	}

	@Override
	public void onEvent(final Event event) {
		final String eventName = event.getName();
		final GameManagementEvent gameEventType = GameManagementEvent.getEventType(eventName);
		if (gameEventType == null) {
			// The broker event is not a relevant tangrams game event; Ignore it
			if (LOGGER.isDebugEnabled()) {
				final String sender = event.getSender();
				final String msg = sender == null
						? String.format("Ignoring received broker event named \"%s\".", eventName)
						: String.format("Ignoring received broker event from \"%s\", named \"%s\".", sender, eventName);
				LOGGER.debug(msg);
			}

		} else {
			final String gameId = event.getString(GameManagementEvent.Attribute.GAME_ID.toString());
			final String submittingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString(), "");
			if (Objects.equals(gameId, this.gameId) && !submittingPlayerId.equals(playerId)) {
				switch (gameEventType) {
				case COMPLETED_TURN_REQUEST: {
					LOGGER.debug("Received game event reporting a completed turn, submitted by \"{}\".",
							submittingPlayerId);
					final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
					controller.notifyTurnComplete(submittingPlayerId, move);
					break;
				}
				case GAME_READY_RESPONSE: {
					LOGGER.info("The server notified that game \"{}\" is ready.", gameId);
					final GameStateDescription gameDesc = (GameStateDescription) event
							.get(GameManagementEvent.Attribute.GAME_STATE.toString());
					setupGame(gameDesc);
					break;
				}
				case NEXT_TURN_REQUEST: {
					LOGGER.debug("Received game event reporting the submission of a new move, submitted by \"{}\".",
							submittingPlayerId);
					final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
					controller.notifyNextMove(submittingPlayerId, move);
					break;
				}
				case PLAYER_JOIN_RESPONSE: {
					final String joinTime = event.getString(GameManagementEvent.Attribute.TIMESTAMP.toString());
					LOGGER.debug("Received game event reporting that \"{}\" has joined the game at {}.",
							submittingPlayerId, joinTime);
					if (controller == null) {
						LOGGER.debug("Game controller not yet set; Not notifying controller of joined player.");
					} else {
						controller.notifyPlayerJoined(submittingPlayerId, Timestamp.valueOf(joinTime).getTime());
					}
					break;
				}
				case SELECTION_REJECTION: {
					LOGGER.debug("Received game event reporting that \"{}\" rejected a selection.", submittingPlayerId);
					final Selection selection = (Selection) event
							.get(GameManagementEvent.Attribute.SELECTION.toString());
					controller.notifySelectionRejected(submittingPlayerId, selection);
					break;
				}
				case SELECTION_REQUEST: {
					LOGGER.debug("Received game event reporting selection info for \"{}\".", submittingPlayerId);
					final Selection selection = (Selection) event
							.get(GameManagementEvent.Attribute.SELECTION.toString());
					controller.notifyPlayerSelection(submittingPlayerId, selection);
					break;
				}
				default: {
					LOGGER.debug("Ignoring received game event type \"{}\".", gameEventType);
					break;
				}
				}
			}
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.iristk.GameManagementClient#rejectSelection(
	 * java.lang.Integer, se.kth.speech.coin.tangrams.iristk.events.Area2D)
	 */
	@Override
	public void rejectSelection(final Integer pieceId, final Area2D area) {
		final Event request = createPlayerEvent(GameManagementEvent.SELECTION_REJECTION);
		request.put(GameManagementEvent.Attribute.SELECTION.toString(), new Selection(pieceId, area));
		LOGGER.info("Sending broker event for rejecting selection of piece \"{}\" by \"{}\".", pieceId, playerId);
		send(request);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.iristk.GameManagementClient#requestJoinGame()
	 */
	@Override
	public void requestJoinGame() {
		final Event request = createPlayerEvent(GameManagementEvent.PLAYER_JOIN_REQUEST);
		LOGGER.info("Sending broker event for requesting to join game \"{}\".", gameId);
		send(request);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.iristk.GameManagementClient#requestNextMove(
	 * se.kth.speech.coin.tangrams.iristk.events.Move)
	 */
	@Override
	public void requestNextMove(final Move move) {
		final Event request = createPlayerEvent(GameManagementEvent.NEXT_TURN_REQUEST);
		request.put(GameManagementEvent.Attribute.MOVE.toString(), move);
		LOGGER.info("Sending broker event for requesting to complete turn by player \"{}\".", playerId);
		send(request);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.iristk.GameManagementClient#requestSelection(
	 * java.lang.Integer, se.kth.speech.coin.tangrams.iristk.events.Area2D)
	 */
	@Override
	public void requestSelection(final Integer pieceId, final Area2D area) {
		final Event request = createPlayerEvent(GameManagementEvent.SELECTION_REQUEST);
		request.put(GameManagementEvent.Attribute.SELECTION.toString(), new Selection(pieceId, area));
		LOGGER.info("Sending broker event for selecting piece \"{}\" by \"{}\".", pieceId, playerId);
		send(request);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.iristk.GameManagementClient#
	 * requestTurnCompletion(se.kth.speech.coin.tangrams.iristk.events.Move)
	 */
	@Override
	public void requestTurnCompletion(final Move move) {
		final Event request = createPlayerEvent(GameManagementEvent.COMPLETED_TURN_REQUEST);
		request.put(GameManagementEvent.Attribute.MOVE.toString(), move);
		LOGGER.info("Sending broker event for requesting to complete turn by player \"{}\".", playerId);
		send(request);
	}

	private Event createPlayerEvent(final GameManagementEvent eventType) {
		final Event result = eventType.createEvent(gameId);
		result.put(GameManagementEvent.Attribute.PLAYER_ID.toString(), playerId);
		return result;
	}

	/**
	 * @param controller
	 *            the controller to set
	 */
	private void setController(final Controller controller) {
		if (this.controller == null) {
			this.controller = controller;
		} else {
			throw new IllegalStateException("The controller reference can be set only once.");
		}
	}

	private void setupGame(final GameStateDescription gameDesc) {
		final ModelDescription modelDesc = gameDesc.getModelDescription();
		final SpatialMatrix<Integer> model = GAME_MODEL_MATRIX_UNMARSHALLER.apply(modelDesc);

		final PlayerRole role = gameDesc.getPlayerRoles().inverse().get(playerId);

		final GameplayController controller = new GameplayController(model, playerId, role, this);
		setController(controller);

		final ImageVisualizationInfoDescription imgVizDesc = gameDesc.getImageVisualizationInfoDescription();
		final ImageVisualizationInfo imgVizInfo = imgVizDesc.toHashable();

		final BiMap<PlayerRole, String> playerRoles = gameDesc.getPlayerRoles();
		newGameHandler.accept(new GameState(controller, imgVizInfo, playerRoles, new Random(gameDesc.getSeed()),
				gameDesc.getOccupiedGridArea(), gameDesc.allowFailedPlacements()));
	}

}
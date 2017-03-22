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

import com.google.common.collect.BiMap;

import iristk.system.Event;
import iristk.system.IrisModule;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;

public final class GameManagementClientModule extends IrisModule {

	// private static final Logger LOGGER =
	// LoggerFactory.getLogger(GameManagementClientModule.class);
	private static final MyLogger LOGGER = new MyLogger();

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
			if (Objects.equals(gameId, this.gameId)
					&& !event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString(), "").equals(playerId)) {
				switch (gameEventType) {
				case COMPLETED_TURN_REQUEST: {
					final String submittingPlayerId = event
							.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
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
					final String submittingPlayerId = event
							.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
					LOGGER.debug("Received game event reporting the submission of a new move, submitted by \"{}\".",
							submittingPlayerId);
					final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
					controller.notifyNextMove(submittingPlayerId, move);
					break;
				}
				case PLAYER_JOIN_RESPONSE: {
					final String joinedPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
					final String joinTime = event.getString(GameManagementEvent.Attribute.TIMESTAMP.toString());
					LOGGER.debug("Received game event reporting that \"{}\" has joined the game at {}.", joinedPlayerId,
							joinTime);
					if (controller == null) {
						LOGGER.debug("Game controller not yet set; Not notifying controller of joined player.");
					} else {
						controller.notifyPlayerJoined(joinedPlayerId, Timestamp.valueOf(joinTime).getTime());
					}
					break;
				}
				case SELECTION_REJECTION: {
					final String rejectingPlayerId = event
							.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
					LOGGER.debug("Received game event reporting that \"{}\" rejected a selection.", rejectingPlayerId);
					final Selection selection = (Selection) event
							.get(GameManagementEvent.Attribute.SELECTION.toString());
					controller.notifySelectionRejected(rejectingPlayerId, selection);
					break;
				}
				case SELECTION_REQUEST: {
					final String selectingPlayerId = event
							.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
					LOGGER.debug("Received game event reporting selection info for \"{}\".", selectingPlayerId);
					final Selection selection = (Selection) event
							.get(GameManagementEvent.Attribute.SELECTION.toString());
					controller.notifyPlayerSelection(selectingPlayerId, selection);
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

	public void rejectSelection(final Integer pieceId, final Area2D area) {
		final Event request = createPlayerEvent(GameManagementEvent.SELECTION_REJECTION);
		request.put(GameManagementEvent.Attribute.SELECTION.toString(), new Selection(pieceId, area));
		LOGGER.info("Sending broker event for rejecting selection of piece \"{}\" by \"{}\".",
				new Object[] { pieceId, playerId });
		send(request);
	}

	public void requestJoinGame() {
		final Event request = createPlayerEvent(GameManagementEvent.PLAYER_JOIN_REQUEST);
		LOGGER.info("Sending broker event for requesting to join game \"{}\".", gameId);
		send(request);
	}

	public void requestNextMove(final Move move) {
		final Event request = createPlayerEvent(GameManagementEvent.NEXT_TURN_REQUEST);
		request.put(GameManagementEvent.Attribute.MOVE.toString(), move);
		LOGGER.info("Sending broker event for requesting to complete turn by player \"{}\".", playerId);
		send(request);
	}

	public void requestSelection(final Integer pieceId, final Area2D area) {
		final Event request = createPlayerEvent(GameManagementEvent.SELECTION_REQUEST);
		request.put(GameManagementEvent.Attribute.SELECTION.toString(), new Selection(pieceId, area));
		LOGGER.info("Sending broker event for selecting piece \"{}\" by \"{}\".", new Object[] { pieceId, playerId });
		send(request);
	}

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
		final SpatialMatrix<Integer> model = GameStateUnmarshalling.createModel(modelDesc);
		final PlayerRole role = gameDesc.getPlayerRoles().inverse().get(playerId);
		final Controller controller = new Controller(model, playerId, role, this);
		setController(controller);
		final BiMap<PlayerRole, String> playerRoles = gameDesc.getPlayerRoles();
		newGameHandler.accept(new GameState(controller, playerRoles, new Random(gameDesc.getSeed()),
				gameDesc.getOccupiedGridArea(), gameDesc.allowFailedPlacements()));
	}

}
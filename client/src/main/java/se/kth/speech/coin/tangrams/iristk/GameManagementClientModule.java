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
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Predicate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import iristk.system.IrisModule;
import se.kth.speech.coin.tangrams.game.LocalController;
import se.kth.speech.coin.tangrams.game.PlayerJoinTime;
import se.kth.speech.coin.tangrams.game.RemoteController;
import se.kth.speech.coin.tangrams.iristk.events.ActivePlayerChange;
import se.kth.speech.coin.tangrams.iristk.events.CoordinatePoint2D;
import se.kth.speech.coin.tangrams.iristk.events.GameEnding;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.events.Turn;
import se.kth.speech.matrix.SpatialMatrix;

public final class GameManagementClientModule extends IrisModule {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameManagementClientModule.class);

	private final Consumer<? super GameEnding> gameEndingHook;

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

	private RemoteController<?> remoteController;

	public GameManagementClientModule(final String gameId, final String playerId,
			final Consumer<? super GameEnding> gameEndingHook, final Consumer<? super GameState> newGameHandler) {
		this.gameId = gameId;
		this.playerId = playerId;
		this.gameEndingHook = gameEndingHook;
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
			if (Objects.equals(gameId, this.gameId)) {
				switch (gameEventType) {
				case GAME_OVER_RESPONSE: {
					LOGGER.info("The server notified that game \"{}\" is over.", gameId);
					final GameEnding gameEnding = (GameEnding) event
							.get(GameManagementEvent.Attribute.GAME_STATE.toString());
					remoteController.notifyGameOver(gameEnding);
					gameEndingHook.accept(gameEnding);
					break;
				}
				case GAME_READY_RESPONSE: {
					LOGGER.info("The server notified that game \"{}\" is ready.", gameId);
					final GameStateDescription gameDesc = (GameStateDescription) event
							.get(GameManagementEvent.Attribute.GAME_STATE.toString());
					setupGame(gameDesc);
					break;
				}
				case PLAYER_JOIN_RESPONSE: {
					final String joinedPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
					final String joinTime = event.getString(GameManagementEvent.Attribute.TIMESTAMP.toString());
					LOGGER.debug("Received game event reporting that \"{}\" has joined the game at {}.", joinedPlayerId,
							joinTime);
					if (remoteController == null) {
						LOGGER.debug("Game controller not yet set; Not notifying controller of joined player.");
					} else {
						remoteController.notifyPlayerJoined(
								new PlayerJoinTime(joinedPlayerId, Timestamp.valueOf(joinTime).getTime()));
					}

					break;
				}
				case SELECTION_REQUEST: {
					final String selectingPlayerId = event
							.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
					LOGGER.debug("Received game event reporting selection info for \"{}\".", selectingPlayerId);
					final CoordinatePoint2D coords = (CoordinatePoint2D) event
							.get(GameManagementEvent.Attribute.COORDS.toString());
					remoteController.notifyPlayerSelection(new Selection(selectingPlayerId, coords));
					break;
				}
				case TURN_RESPONSE: {
					final Turn turn = (Turn) event.get(GameManagementEvent.Attribute.TURN.toString());
					remoteController.notifyPlayerTurn(turn);
					final ActivePlayerChange playerIds = (ActivePlayerChange) event
							.get(GameManagementEvent.Attribute.ACTIVE_PLAYER_CHANGE.toString());
					final String newActivePlayerId = playerIds.getNewActivePlayerId();
					LOGGER.debug("The server notified that player \"{}\" is now active.", newActivePlayerId);
					remoteController.notifyNewActivePlayer(playerIds);
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

	public void requestJoinGame() {
		final Event request = createPlayerEvent(GameManagementEvent.PLAYER_JOIN_REQUEST);
		LOGGER.info("Sending broker event for requesting to join game \"{}\".", gameId);
		send(request);
	}

	public void requestTurnCompletion(final Move move) {
		final Event request = createPlayerEvent(GameManagementEvent.TURN_REQUEST);
		request.put(GameManagementEvent.Attribute.MOVE.toString(), move);
		LOGGER.info("Sending broker event for requesting to complete turn by player \"{}\".", playerId);
		send(request);
	}

	public void requestUserSelection(final CoordinatePoint2D coords) {
		final Event request = createPlayerEvent(GameManagementEvent.SELECTION_REQUEST);
		request.put(GameManagementEvent.Attribute.COORDS.toString(), coords);
		LOGGER.info("Sending broker event for selecting {} by \"{}\".", new Object[] { coords.getCoords(), playerId });
		send(request);
	}

	/**
	 * @param controller
	 *            the controller to set
	 */
	private void addNewGameRemoteController(final RemoteController<?> controller) {
		if (remoteController == null) {
			remoteController = controller;
		} else {
			throw new IllegalStateException("The remote controller reference can be set only once.");
		}
	}

	private Event createPlayerEvent(final GameManagementEvent eventType) {
		final Event result = eventType.createEvent(gameId);
		result.put(GameManagementEvent.Attribute.PLAYER_ID.toString(), playerId);
		return result;
	}

	private void setupGame(final GameStateDescription gameDesc) {
		final ModelDescription modelDesc = gameDesc.getModelDescription();
		final SpatialMatrix<Integer> model = GameStateUnmarshalling.createModel(modelDesc);
		final boolean isActive = playerId.equals(gameDesc.getActivePlayerId());
		final LocalController<Integer> localController = new LocalController<>(model, playerId, isActive,
				this::requestTurnCompletion, this::requestUserSelection);
		final Consumer<ActivePlayerChange> controllerActivationHook = handoff -> {
			final boolean shouldControllerBeEnabled = localController.getPlayerId()
					.equals(handoff.getNewActivePlayerId());
			LOGGER.debug("Setting local controller \"enabled\" status for player \"{}\" to {}.",
					new Object[] { playerId, shouldControllerBeEnabled });
			localController.setEnabled(shouldControllerBeEnabled);
		};
		final Predicate<String> foreignPlayerIdPredicate = pid -> !playerId.equals(pid);
		final RemoteController<Integer> remoteController = new RemoteController<>(model, controllerActivationHook,
				foreignPlayerIdPredicate);
		addNewGameRemoteController(remoteController);
		final List<String> playerIds = gameDesc.getPlayerIds();
		newGameHandler.accept(new GameState(localController, remoteController, playerIds, gameDesc.getSeed()));
	}

}
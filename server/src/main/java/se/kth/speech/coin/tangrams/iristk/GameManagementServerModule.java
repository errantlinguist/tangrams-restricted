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

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import iristk.system.IrisModule;
import iristk.util.Pair;
import se.kth.speech.coin.tangrams.Game;
import se.kth.speech.coin.tangrams.game.PlayerJoinTime;
import se.kth.speech.coin.tangrams.game.RemoteController;
import se.kth.speech.coin.tangrams.iristk.events.ActivePlayerChange;
import se.kth.speech.coin.tangrams.iristk.events.CoordinatePoint;
import se.kth.speech.coin.tangrams.iristk.events.GameEnding;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

public final class GameManagementServerModule extends IrisModule {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameManagementServerModule.class);

	private static final int MIN_GAME_PLAYER_COUNT = 2;

	private static Event createGameStateDescriptionEvent(final String gameId,
			final Pair<Game<Integer>, ActivePlayerTracker> gameState) {
		final GameStateDescription gameDesc = new GameStateDescription();
		{
			final ActivePlayerTracker activePlayerTracker = gameState.getSecond();
			final Collection<PlayerJoinTime> joinedPlayers = activePlayerTracker.getJoinedPlayers();
			final Stream<String> playerIdStream = joinedPlayers.stream().map(PlayerJoinTime::getPlayerId);
			final List<String> playerIdList = playerIdStream.collect(Collectors.toList());
			gameDesc.setPlayerIds(playerIdList);

			final String startingPlayerId;
			{
				String activePlayerId = activePlayerTracker.getActivePlayerId();
				if (activePlayerId == null) {
					// The game has not started yet; Cycle to the
					// first-joined
					// player as the first active player
					activePlayerId = activePlayerTracker.cycleActivePlayer();
				}
				startingPlayerId = activePlayerId;
			}
			gameDesc.setActivePlayerId(startingPlayerId);

			final Game<Integer> game = gameState.getFirst();
			gameDesc.setModelDescription(new ModelDescription(game.getRemoteController().getModel()));
			final List<Integer> winningConfig = game.getWinningModel().getCoordinateOccupants().getValues();
			gameDesc.setWinningConfiguration(
					winningConfig.stream().map(Objects::toString).collect(Collectors.toList()));
			gameDesc.setSeed(game.getSeed());
		}

		final Event result = GameManagementEvent.GAME_READY_RESPONSE.createEvent(gameId);
		result.put(GameManagementEvent.Attribute.GAME_STATE.toString(), gameDesc);

		return result;
	}

	private static boolean isReady(final Pair<? extends Game<?>, ActivePlayerTracker> gameState) {
		return gameState.getSecond().getPlayerCount() >= MIN_GAME_PLAYER_COUNT;
	}

	private final Function<String, Pair<Game<Integer>, ActivePlayerTracker>> gameStateGetter;

	public GameManagementServerModule(final Function<String, Game<Integer>> gameFactory) {
		gameStateGetter = new Function<String, Pair<Game<Integer>, ActivePlayerTracker>>() {

			private final ConcurrentMap<String, Pair<Game<Integer>, ActivePlayerTracker>> gamePlayers = new ConcurrentHashMap<>();

			@Override
			public Pair<Game<Integer>, ActivePlayerTracker> apply(final String gameId) {
				return gamePlayers.compute(gameId, (key, oldValue) -> {
					final Pair<Game<Integer>, ActivePlayerTracker> result;
					if (oldValue == null) {
						final Game<Integer> joinedGame = gameFactory.apply(key);
						LOGGER.info("No known state for game \"{}\"; Creating new managed state.", key);
						result = new Pair<>(joinedGame, new ActivePlayerTracker());
					} else {
						result = oldValue;
					}
					return result;
				});
			}
		};
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

			switch (gameEventType) {
			case PLAYER_JOIN_REQUEST: {
				final String joinedPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
				LOGGER.debug("Received join request from player \"{}\" for game \"{}\".",
						new Object[] { joinedPlayerId, gameId });
				final Pair<Game<Integer>, ActivePlayerTracker> gameState = gameStateGetter.apply(gameId);
				final boolean isGameAlreadyReady = isReady(gameState);

				final long joinTime = System.currentTimeMillis();
				final ActivePlayerTracker activePlayerTracker = gameState.getSecond();
				if (activePlayerTracker.addPlayer(new PlayerJoinTime(joinedPlayerId, joinTime))) {
					LOGGER.debug("Added player \"{}\" to game \"{}\".", new Object[] { joinedPlayerId, gameId });
					{
						final Event joinResponse = GameManagementEvent.PLAYER_JOIN_RESPONSE.createEvent(gameId);
						joinResponse.put(GameManagementEvent.Attribute.PLAYER_ID.toString(), joinedPlayerId);
						final String joinTimestamp = getSystem().getTimestamp(joinTime);
						joinResponse.put(GameManagementEvent.Attribute.TIMESTAMP.toString(), joinTimestamp);
						LOGGER.info("Sending broker event acknowledging the joining of player \"{}\" to game \"{}\".",
								new Object[] { joinedPlayerId, gameId });
						send(joinResponse);
					}

					if (isGameAlreadyReady) {
						if (isReady(gameState)) {
							// TODO: add logic for adding users to an
							// already-running game
							throw new RuntimeException(
									"Adding new players to already-running games is (currently) not supported.");
						} else {
							throw new AssertionError("A previously-ready game should never become unready.");
						}
					} else {
						// Check if the game is now ready
						if (isReady(gameState)) {
							// Send a broker event signalling "game (now) ready"
							final Event readySignal = createGameStateDescriptionEvent(gameId, gameState);
							LOGGER.info("Sending broker event signalling that game \"{}\" is ready.", gameId);
							send(readySignal);
						} else {
							LOGGER.debug("Game \"{}\" (still) not ready: {} already-joined player(s).", gameId,
									activePlayerTracker.getPlayerCount());
						}
					}

				} else {
					LOGGER.warn(
							"Player \"{}\" requested to join game \"{}\" but has already joined; Ignoring broker event.",
							joinedPlayerId, gameId);
				}

				break;
			}
			case PLAYER_LEAVE_REQUEST : {
				final String leavingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
				LOGGER.debug("Received leave request from player \"{}\" for game \"{}\".",
						new Object[] { leavingPlayerId, gameId });
				final Pair<Game<Integer>, ActivePlayerTracker> gameState = gameStateGetter.apply(gameId);
				final ActivePlayerTracker activePlayerTracker = gameState.getSecond();
				// TODO: Finish
				break;
			}
			case SELECTION_REQUEST: {
				if (LOGGER.isDebugEnabled()) {
					final String selectingPlayerId = event
							.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
					final CoordinatePoint coords = (CoordinatePoint) event
							.get(GameManagementEvent.Attribute.COORDS.toString());
					LOGGER.debug("Received game event denoting that \"{}\" toggled selection at {}.", selectingPlayerId,
							Arrays.toString(coords.getCoords()));
				}
				break;
			}
			case TURN_REQUEST: {
				LOGGER.debug("Received broker turn event for game \"{}\".", gameId);
				final Pair<Game<Integer>, ActivePlayerTracker> gameState = gameStateGetter.apply(gameId);
				final Game<Integer> game = gameState.getFirst();

				final ActivePlayerTracker activePlayerTracker = gameState.getSecond();
				final String playerIdField = GameManagementEvent.Attribute.PLAYER_ID.toString();
				final String turnPlayerId = event.getString(playerIdField);
				final String oldActivePlayerId = activePlayerTracker.getActivePlayerId();
				if (!turnPlayerId.equals(oldActivePlayerId)) {
					throw new IllegalStateException("Models of server and client(s) have gone out of sync.");
				}

				{
					final Event turnResponse = GameManagementEvent.TURN_RESPONSE.createEvent(gameId);
					{
						turnResponse.put(playerIdField, turnPlayerId);
						{
							final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
							final RemoteController<Integer> controller = game.getRemoteController();
							final int turnNo = controller.getMoveCount() + 1;
							final Turn turn = new Turn(turnPlayerId, move, turnNo);
							controller.notifyPlayerTurn(turn);
							turnResponse.put(GameManagementEvent.Attribute.TURN.toString(), turn);
						}
					}

					final String newActivePlayerId;
					// If the game has been won, don't allow any player to
					// become active again
					if (game.isWon()) {
						newActivePlayerId = null;
					} else {
						newActivePlayerId = activePlayerTracker.cycleActivePlayer();
					}
					turnResponse.put(GameManagementEvent.Attribute.ACTIVE_PLAYER_CHANGE.toString(),
							new ActivePlayerChange(oldActivePlayerId, newActivePlayerId));
					LOGGER.info(
							"Sending broker event for game \"{}\" signalling a move, after which player \"{}\" is now active.",
							new Object[] { gameId, newActivePlayerId });
					send(turnResponse);
				}

				// Check if the game has now been won
				if (game.isWon()) {
					final Event gameOverResponse = GameManagementEvent.GAME_OVER_RESPONSE.createEvent(gameId);
					gameOverResponse.put(GameManagementEvent.Attribute.GAME_STATE.toString(), new GameEnding(
							turnPlayerId, game.getRemoteController().getMoveCount(), GameEnding.Outcome.WIN));
					LOGGER.info("Sending broker event for game \"{}\" signalling that the game is over.", gameId);
					send(gameOverResponse);
				}
				break;
			}
			default: {
				LOGGER.debug("Ignoring broker event for game \"{}\" of event type \"{}\".",
						new Object[] { gameId, gameEventType });
				break;
			}
			}
		}
	}

}
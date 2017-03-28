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
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import iristk.system.IrisModule;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.game.Game;
import se.kth.speech.coin.tangrams.game.GameFactory;
import se.kth.speech.coin.tangrams.game.GameFactory.Parameter;
import se.kth.speech.coin.tangrams.game.PlayerRole;

public final class GameManagementServerModule extends IrisModule {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameManagementServerModule.class);

	private static final int MIN_GAME_PLAYER_COUNT = 2;

	private static final List<PlayerRole> PLAYER_ROLE_FILL_ORDER = Arrays.asList(PlayerRole.MOVE_SUBMISSION,
			PlayerRole.WAITING_FOR_NEXT_MOVE);

	private static boolean isReady(final Game<?> gameState) {
		return gameState.getPlayerRoles().keySet().size() >= MIN_GAME_PLAYER_COUNT;
	}

	private final GameFactory gameFactory;

	private final ConcurrentMap<String, Game<Integer>> newGames;

	public GameManagementServerModule(final GameFactory gameFactory) {
		this(gameFactory, new ConcurrentHashMap<>());
	}

	public GameManagementServerModule(final GameFactory gameFactory,
			final ConcurrentMap<String, Game<Integer>> newGames) {
		this.gameFactory = gameFactory;
		this.newGames = newGames;
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
				LOGGER.info("Received join request from player \"{}\" for game \"{}\".",
						new Object[] { joinedPlayerId, gameId });
				newGames.compute(gameId, (key, oldVal) -> {
					final Game<Integer> newVal;

					final boolean isReady;
					final Game<Integer> gameAddedTo;
					if (oldVal == null) {
						LOGGER.info("No known state for game \"{}\"; Creating new managed state.", key);
						gameAddedTo = gameFactory.apply(key);
						isReady = addPlayerUnchecked(gameId, gameAddedTo, joinedPlayerId);
					} else {
						gameAddedTo = oldVal;
						isReady = addPlayer(gameId, oldVal, joinedPlayerId);
					}
					// Remove the now-ready game state from the map in
					// order to be able to let more players play new
					// instances of games which happen to have the same
					// game ID
					newVal = isReady ? null : gameAddedTo;
					return newVal;
				});
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

	private boolean addPlayer(final String gameId, final Game<Integer> gameState, final String joinedPlayerId) {
		final boolean result;
		final Map<PlayerRole, String> playerRoles = gameState.getPlayerRoles();
		if (playerRoles.containsValue(joinedPlayerId)) {
			LOGGER.warn("Player \"{}\" requested to join game \"{}\" but has already joined; Ignoring broker event.",
					joinedPlayerId, gameId);
			result = false;
		} else {
			result = addPlayerUnchecked(gameId, gameState, joinedPlayerId);
		}
		return result;
	}

	private boolean addPlayerUnchecked(final String gameId, final Game<Integer> gameState,
			final String joinedPlayerId) {
		final boolean result;

		final Map<PlayerRole, String> playerRoles = gameState.getPlayerRoles();
		final Optional<PlayerRole> foundRoleToFill = PLAYER_ROLE_FILL_ORDER.stream()
				.filter(role -> !playerRoles.containsKey(role)).findFirst();
		final PlayerRole roleToFill = foundRoleToFill.get();
		final String oldRolePlayerId = playerRoles.put(roleToFill, joinedPlayerId);
		assert oldRolePlayerId == null;
		LOGGER.debug("Added player \"{}\" to game \"{}\" with role {}.",
				new Object[] { joinedPlayerId, gameId, roleToFill });
		{
			final Event joinResponse = GameManagementEvent.PLAYER_JOIN_RESPONSE.createEvent(gameId);
			joinResponse.put(GameManagementEvent.Attribute.PLAYER_ID.toString(), joinedPlayerId);
			final long joinTime = System.currentTimeMillis();
			final String joinTimestamp = getSystem().getTimestamp(joinTime);
			joinResponse.put(GameManagementEvent.Attribute.TIMESTAMP.toString(), joinTimestamp);
			LOGGER.info("Sending broker event acknowledging the joining of player \"{}\" to game \"{}\".",
					new Object[] { joinedPlayerId, gameId });
			send(joinResponse);
		}
		// Check if the game is now ready
		if (result = isReady(gameState)) {
			// Send a broker event signalling "game (now)
			// ready"
			final Event readySignal = createGameStateDescriptionEvent(gameId, gameState);
			LOGGER.info("Sending broker event signalling that game \"{}\" is ready.", gameId);
			send(readySignal);

		} else {
			LOGGER.debug("Game \"{}\" (still) not ready: {} already-joined player(s).", gameId, playerRoles.size());
		}

		return result;
	}

	private Event createGameStateDescriptionEvent(final String gameId, final Game<Integer> gameState) {
		final GameStateDescription gameDesc = new GameStateDescription();

		Long seed;
		try {
			seed = Long.valueOf(gameId);
		} catch (final NumberFormatException nfe) {
			final Map<Parameter, Object> gameParams = gameFactory.getGameParamMapFactory().apply(gameId);
			final Parameter seedParam = Parameter.SEED;
			final Object seedParamVal = gameParams.get(seedParam);
			if (seedParamVal == null) {
				throw new IllegalArgumentException("Could not parse random seed from game ID \"" + gameId + "\".");
			} else {
				seed = (Long) seedParamVal;
			}
		}
		gameDesc.setSeed(seed);

		final Map<PlayerRole, String> playerRoles = gameState.getPlayerRoles();
		gameDesc.setPlayerRoles(playerRoles);

		gameDesc.setModelDescription(new ModelDescription(gameState.getModel().getPositionMatrix()));

		final ImageVisualizationInfo imgVizInfo = gameState.getImgVisualizationInfo();
		final ImageVisualizationInfoDescription imgVizInfoDesc = new ImageVisualizationInfoDescription(imgVizInfo);
		gameDesc.setImageVisualizationInfoDescription(imgVizInfoDesc);

		final Event result = GameManagementEvent.GAME_READY_RESPONSE.createEvent(gameId);
		result.put(GameManagementEvent.Attribute.GAME_STATE.toString(), gameDesc);

		return result;
	}

}
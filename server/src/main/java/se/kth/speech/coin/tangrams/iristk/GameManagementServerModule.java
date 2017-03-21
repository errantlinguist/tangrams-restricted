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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.BiMap;

import iristk.system.Event;
import iristk.system.IrisModule;
import se.kth.speech.coin.tangrams.Game;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.PlayerRoleChange;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

public final class GameManagementServerModule extends IrisModule {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameManagementServerModule.class);

	private static final int MIN_GAME_PLAYER_COUNT = 2;

	private static final List<PlayerRole> PLAYER_ROLE_FILL_ORDER = Arrays.asList(PlayerRole.TURN_SUBMISSION,
			PlayerRole.SELECTING);

	private static Event createGameStateDescriptionEvent(final String gameId, final Game<Integer> gameState) {
		final GameStateDescription gameDesc = new GameStateDescription();
		gameDesc.setSeed(Long.parseLong(gameId));

		{
			final Map<PlayerRole, String> playerRoles = gameState.getPlayerRoles();
			gameDesc.setPlayerRoles(playerRoles);

			gameDesc.setModelDescription(new ModelDescription(gameState.getModel().getPositionMatrix()));
		}

		final Event result = GameManagementEvent.GAME_READY_RESPONSE.createEvent(gameId);
		result.put(GameManagementEvent.Attribute.GAME_STATE.toString(), gameDesc);

		return result;
	}

	private static boolean isReady(final Game<?> gameState) {
		return gameState.getPlayerRoles().keySet().size() >= MIN_GAME_PLAYER_COUNT;
	}

	private final Function<String, Game<Integer>> gameStateGetter;

	public GameManagementServerModule(final Function<String, Game<Integer>> gameFactory) {
		gameStateGetter = new Function<String, Game<Integer>>() {

			private final ConcurrentMap<String, Game<Integer>> gamePlayers = new ConcurrentHashMap<>();

			@Override
			public Game<Integer> apply(final String gameId) {
				return gamePlayers.compute(gameId, (key, oldValue) -> {
					final Game<Integer> result;
					if (oldValue == null) {
						final Game<Integer> joinedGame = gameFactory.apply(key);
						LOGGER.info("No known state for game \"{}\"; Creating new managed state.", key);
						result = joinedGame;
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
				final Game<Integer> gameState = gameStateGetter.apply(gameId);
				final boolean isGameAlreadyReady = isReady(gameState);
				final long joinTime = System.currentTimeMillis();
				final Map<PlayerRole, String> playerRoles = gameState.getPlayerRoles();
				PlayerRole filledRole = null;
				if (playerRoles.containsValue(joinedPlayerId)) {
					LOGGER.warn(
							"Player \"{}\" requested to join game \"{}\" but has already joined; Ignoring broker event.",
							joinedPlayerId, gameId);
				} else {
					for (final PlayerRole roleToFill : PLAYER_ROLE_FILL_ORDER) {
						if (!playerRoles.containsKey(roleToFill)) {
							filledRole = roleToFill;
							break;
						}
					}
					if (filledRole == null) {
						throw new RuntimeException(
								String.format("No more player roles to assign for game \"%s\".", gameId));
					} else {
						playerRoles.put(filledRole, joinedPlayerId);
						LOGGER.debug("Added player \"{}\" to game \"{}\" with role {}.",
								new Object[] { joinedPlayerId, gameId, filledRole });

						{
							final Event joinResponse = GameManagementEvent.PLAYER_JOIN_RESPONSE.createEvent(gameId);
							joinResponse.put(GameManagementEvent.Attribute.PLAYER_ID.toString(), joinedPlayerId);
							final String joinTimestamp = getSystem().getTimestamp(joinTime);
							joinResponse.put(GameManagementEvent.Attribute.TIMESTAMP.toString(), joinTimestamp);
							LOGGER.info(
									"Sending broker event acknowledging the joining of player \"{}\" to game \"{}\".",
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
								// Send a broker event signalling "game (now)
								// ready"
								final Event readySignal = createGameStateDescriptionEvent(gameId, gameState);
								LOGGER.info("Sending broker event signalling that game \"{}\" is ready.", gameId);
								send(readySignal);
							} else {
								LOGGER.debug("Game \"{}\" (still) not ready: {} already-joined player(s).", gameId,
										playerRoles.keySet().size());
							}
						}
					}
				}

				break;
			}
			/*
			case NEXT_TURN_REQUEST: {
				LOGGER.debug("Received broker \"next turn\" event for game \"{}\".", gameId);
				final Game<Integer> gameState = gameStateGetter.apply(gameId);
				final String playerIdField = GameManagementEvent.Attribute.PLAYER_ID.toString();
				final String requestingPlayerId = event.getString(playerIdField);
				final Map<String, PlayerRole> playerRoles = gameState.getPlayerRoles().inverse();
				final PlayerRole role = playerRoles.get(requestingPlayerId);
				if (PlayerRole.TURN_SUBMISSION.equals(role)) {
					final Event response = GameManagementEvent.NEXT_TURN_RESPONSE.createEvent(gameId);
					response.put(playerIdField, requestingPlayerId);
					final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
					final int turnNo = gameState.getTurnCount().get();
					final Turn turn = new Turn(requestingPlayerId, move, turnNo);
					response.put(GameManagementEvent.Attribute.TURN.toString(), turn);
					LOGGER.info("Sending broker event for game \"{}\" signalling the next move (move no. {}).",
							new Object[] { gameId, turn.getSequenceNumber() });
					send(response);
				} else {
					LOGGER.warn(
							"The player does not have the appropriate role ({}) to request the submission of the next move; Ignoring.",
							role);
				}
				break;
			}
			*/
			case COMPLETED_TURN_REQUEST: {
				LOGGER.debug("Received broker \"next turn\" event for game \"{}\".", gameId);
				final Game<Integer> gameState = gameStateGetter.apply(gameId);
				final String playerIdField = GameManagementEvent.Attribute.PLAYER_ID.toString();
				final String requestingPlayerId = event.getString(playerIdField);
				final BiMap<PlayerRole, String> playerRoles = gameState.getPlayerRoles();
				final PlayerRole oldRole = playerRoles.inverse().get(requestingPlayerId);
				final PlayerRole requiredRole = PlayerRole.WAITING_FOR_SELECTION;
				if (requiredRole.equals(oldRole)) {
					final Event response = GameManagementEvent.COMPLETED_TURN_RESPONSE.createEvent(gameId);
					response.put(playerIdField, requestingPlayerId);
					final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
					final int turnNo = gameState.getTurnCount().incrementAndGet();
					final Turn turn = new Turn(requestingPlayerId, move, turnNo);
					response.put(GameManagementEvent.Attribute.TURN.toString(), turn);
					LOGGER.info("Sending broker event for game \"{}\" signalling the next move (move no. {}).",
							new Object[] { gameId, turn.getSequenceNumber() });
					send(response);

					// Change roles
					final PlayerRole switchedRole = PlayerRole.SELECTING;
					final String otherPlayerId = playerRoles.get(switchedRole);
					playerRoles.put(switchedRole, requestingPlayerId);
					playerRoles.put(PlayerRole.TURN_SUBMISSION, otherPlayerId);
					final List<PlayerRoleChange> roleChanges = new ArrayList<>(playerRoles.size());
					playerRoles.forEach((role, playerId) -> {
						final PlayerRoleChange roleChange = new PlayerRoleChange(playerId, role);
						roleChanges.add(roleChange);
					});
					event.put(GameManagementEvent.Attribute.PLAYER_ROLE_CHANGE.toString(), roleChanges);

				} else {
					LOGGER.warn(
							"Player \"{}\" does not have the appropriate role to request the submission of the next move: The role is {} but should be {}; Ignoring.",
							new Object[] { requestingPlayerId, oldRole, requiredRole });
				}
				break;
			}
			case SELECTION_REJECTION: {
				LOGGER.debug("Received broker \"selection rejected\" event for game \"{}\".", gameId);
				final Game<Integer> gameState = gameStateGetter.apply(gameId);
				final String rejectingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
				LOGGER.debug("Received game event reporting selection info for \"{}\".", rejectingPlayerId);
				final Integer pieceId = (Integer) event.get(GameManagementEvent.Attribute.PIECE.toString());

				// Change roles
				// final BiMap<PlayerRole, String> playerRoles =
				// gameState.getPlayerRoles();
				// final String otherPlayerId = playerRoles.get(switchedRole);
				// playerRoles.put(switchedRole, rejectingPlayerId);
				// playerRoles.put(PlayerRole.TURN_SUBMISSION, otherPlayerId);
				// final List<PlayerRoleChange> roleChanges = new
				// ArrayList<>(playerRoles.size());
				// playerRoles.forEach((role, playerId) -> {
				// final PlayerRoleChange roleChange = new
				// PlayerRoleChange(playerId, role);
				// roleChanges.add(roleChange);
				// });
				// event.put(GameManagementEvent.Attribute.PLAYER_ROLE_CHANGE.toString(),
				// roleChanges);
				// controller.notifySelectionRejected(new
				// Selection(rejectingPlayerId, pieceId));
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
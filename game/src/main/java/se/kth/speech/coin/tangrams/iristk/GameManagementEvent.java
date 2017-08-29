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
import java.util.Collections;
import java.util.EnumSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import iristk.system.Event;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 Jan 2017
 *
 */
public enum GameManagementEvent {
	/**
	 * An event requesting the completion of a turn.
	 */
	COMPLETED_TURN_REQUEST("completedturn.request"),
	/**
	 * An event denoting that a given game is ready to be played,
	 * e.g.&nbsp;enough players have joined.
	 */
	GAME_READY_RESPONSE("game.ready"),
	/**
	 * An event representing the submission of the next turn to be completed.
	 */
	NEXT_TURN_REQUEST("nextturn.request"),
	/**
	 * An event representing a request by the sending client to add the given
	 * player to the list of players in a game.
	 */
	PLAYER_JOIN_REQUEST("playerlist.add.request"),
	/**
	 * An event representing a response by the sending client to
	 * {@link #PLAYER_JOIN_REQUEST a request for a current list of the
	 * players active in a game}.
	 */
	PLAYER_JOIN_RESPONSE("playerlist.add.response"),
	/**
	 * An event representing the rejection of a player's selection.
	 */
	SELECTION_REJECTION("selection.rejection"),
	/**
	 * An event representing a player's selection.
	 */
	SELECTION_REQUEST("selection.request");

	public enum Attribute {
		GAME_ID, GAME_STATE, MOVE, PLAYER_ID, SELECTION, TIMESTAMP;
	}

	public static final String EVENT_NAME_QUALIFIER = "tangrams.action";

	private static final Set<GameManagementEvent> GAME_MODEL_STATE_CHANGING_EVENTS = Collections.unmodifiableSet(EnumSet
			.of(GameManagementEvent.GAME_READY_RESPONSE, GameManagementEvent.NEXT_TURN_REQUEST));

	private static final Map<String, GameManagementEvent> NAMED_EVENTS = Arrays.stream(GameManagementEvent.values())
			.collect(Collectors.toMap(GameManagementEvent::getEventName, Function.identity()));

	public static GameManagementEvent getEventType(final Event event) {
		return getEventType(event.getName());
	}

	public static GameManagementEvent getEventType(final String eventName) {
		return NAMED_EVENTS.get(eventName);
	}

	/**
	 * @return the gameModelStateChangingEvents
	 */
	public static Set<GameManagementEvent> getGameModelStateChangingEvents() {
		return GAME_MODEL_STATE_CHANGING_EVENTS;
	}

	private final String eventName;

	private GameManagementEvent(final String eventName) {
		this.eventName = EVENT_NAME_QUALIFIER + '.' + eventName;
	}

	public String getEventName() {
		return eventName;
	}

	Event createEvent(final String gameId) {
		final Event result = new Event(getEventName());
		result.put(Attribute.GAME_ID.toString(), gameId);
		return result;
	}
}

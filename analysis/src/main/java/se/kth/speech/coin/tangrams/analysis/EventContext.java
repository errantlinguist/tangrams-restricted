/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
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
package se.kth.speech.coin.tangrams.analysis;

import java.math.BigDecimal;

import se.kth.speech.TimestampArithmetic;
import se.kth.speech.coin.tangrams.iristk.GameEvent;

final class EventContext {

	private final int entityId;

	private final GameEvent event;

	private final int eventId;

	private final GameContext gameContext;

	private final int gameRoundId;

	private final BigDecimal offsetSecs;

	private final int score;

	EventContext(final int eventId, final GameEvent event, final int gameRoundId, final GameContext gameContext,
			final int entityId, final int score) {
		this.eventId = eventId;
		this.event = event;
		this.gameContext = gameContext;
		this.gameRoundId = gameRoundId;
		offsetSecs = TimestampArithmetic.toDecimalSeconds(gameContext.getOffset());
		this.entityId = entityId;
		this.score = score;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof EventContext)) {
			return false;
		}
		final EventContext other = (EventContext) obj;
		if (entityId != other.entityId) {
			return false;
		}
		if (event == null) {
			if (other.event != null) {
				return false;
			}
		} else if (!event.equals(other.event)) {
			return false;
		}
		if (eventId != other.eventId) {
			return false;
		}
		if (gameContext == null) {
			if (other.gameContext != null) {
				return false;
			}
		} else if (!gameContext.equals(other.gameContext)) {
			return false;
		}
		if (gameRoundId != other.gameRoundId) {
			return false;
		}
		if (score != other.score) {
			return false;
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + entityId;
		result = prime * result + (event == null ? 0 : event.hashCode());
		result = prime * result + eventId;
		result = prime * result + (gameContext == null ? 0 : gameContext.hashCode());
		result = prime * result + gameRoundId;
		result = prime * result + score;
		return result;
	}

	/**
	 * @return the entityId
	 */
	int getEntityId() {
		return entityId;
	}

	/**
	 * @return the event
	 */
	GameEvent getEvent() {
		return event;
	}

	/**
	 * @return the eventId
	 */
	int getEventId() {
		return eventId;
	}

	/**
	 * @return the gameContext
	 */
	GameContext getGameContext() {
		return gameContext;
	}

	/**
	 * @return the gameRoundId
	 */
	int getGameRoundId() {
		return gameRoundId;
	}

	/**
	 * @return the offsetSecs
	 */
	BigDecimal getOffsetSecs() {
		return offsetSecs;
	}

	/**
	 * @return the score
	 */
	int getScore() {
		return score;
	}

}
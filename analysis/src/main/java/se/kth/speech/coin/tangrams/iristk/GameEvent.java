/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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

import java.time.LocalDateTime;
import java.util.Map;

/**
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 19 Oct 2017
 *
 */
public final class GameEvent {

	private final Map<GameManagementEvent.Attribute, Object> gameAttrs;

	private final String id;

	private final String name;

	private final String sender;

	private final String system;

	private final LocalDateTime time;

	public GameEvent(final GameEvent copyee) {
		this(copyee.getName(), copyee.getSender(), copyee.getId(), copyee.getSystem(), copyee.getTime(),
				copyee.getGameAttrs());
	}

	public GameEvent(final String name, final String sender, final String id, final String system,
			final LocalDateTime time, final Map<GameManagementEvent.Attribute, Object> gameAttrs) {
		this.name = name;
		this.sender = sender;
		this.id = id;
		this.system = system;
		this.time = time;
		this.gameAttrs = gameAttrs;
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
		if (!(obj instanceof GameEvent)) {
			return false;
		}
		final GameEvent other = (GameEvent) obj;
		if (gameAttrs == null) {
			if (other.gameAttrs != null) {
				return false;
			}
		} else if (!gameAttrs.equals(other.gameAttrs)) {
			return false;
		}
		if (id == null) {
			if (other.id != null) {
				return false;
			}
		} else if (!id.equals(other.id)) {
			return false;
		}
		if (name == null) {
			if (other.name != null) {
				return false;
			}
		} else if (!name.equals(other.name)) {
			return false;
		}
		if (sender == null) {
			if (other.sender != null) {
				return false;
			}
		} else if (!sender.equals(other.sender)) {
			return false;
		}
		if (system == null) {
			if (other.system != null) {
				return false;
			}
		} else if (!system.equals(other.system)) {
			return false;
		}
		if (time == null) {
			if (other.time != null) {
				return false;
			}
		} else if (!time.equals(other.time)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the gameAttrs
	 */
	public Map<GameManagementEvent.Attribute, Object> getGameAttrs() {
		return gameAttrs;
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getSender() {
		return sender;
	}

	/**
	 * @return the system
	 */
	public String getSystem() {
		return system;
	}

	public LocalDateTime getTime() {
		return time;
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
		result = prime * result + (gameAttrs == null ? 0 : gameAttrs.hashCode());
		result = prime * result + (id == null ? 0 : id.hashCode());
		result = prime * result + (name == null ? 0 : name.hashCode());
		result = prime * result + (sender == null ? 0 : sender.hashCode());
		result = prime * result + (system == null ? 0 : system.hashCode());
		result = prime * result + (time == null ? 0 : time.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(256);
		builder.append("GameEvent [gameAttrs=");
		builder.append(gameAttrs);
		builder.append(", id=");
		builder.append(id);
		builder.append(", name=");
		builder.append(name);
		builder.append(", sender=");
		builder.append(sender);
		builder.append(", system=");
		builder.append(system);
		builder.append(", time=");
		builder.append(time);
		builder.append("]");
		return builder.toString();
	}

}

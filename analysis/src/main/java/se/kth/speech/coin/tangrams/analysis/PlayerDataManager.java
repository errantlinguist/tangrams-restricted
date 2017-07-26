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
package se.kth.speech.coin.tangrams.analysis;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Maps;

import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;
import se.kth.speech.io.RelativePaths;

/**
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 4 May 2017
 *
 */
public final class PlayerDataManager {

	private static final Pattern PLAYER_PROP_NAME_PATTERN = Pattern.compile("player\\.([^\\.]+)\\.(.+)");

	public static PlayerDataManager create(final Properties props, final Path baseDir) {
		final Set<Entry<Object, Object>> propsEntries = props.entrySet();
		final BiMap<String, String> playerIds = HashBiMap.create(propsEntries.size());
		final BiMap<String, String> sourceIds = HashBiMap.create(propsEntries.size());
		final Map<String, Path> eventLogPaths = Maps.newHashMapWithExpectedSize(props.size());
		for (final Entry<Object, Object> prop : props.entrySet()) {
			final String propName = prop.getKey().toString();
			final Matcher playerPropNameMatcher = PLAYER_PROP_NAME_PATTERN.matcher(propName);
			if (playerPropNameMatcher.matches()) {
				final String tupleId = playerPropNameMatcher.group(1);
				final String datumTypeName = playerPropNameMatcher.group(2);
				switch (datumTypeName) {
				case "eventLog": {
					final Path eventLogPath = RelativePaths.resolveIfNotAbsolute(Paths.get(prop.getValue().toString()),
							baseDir);
					eventLogPaths.put(tupleId, eventLogPath);
					break;
				}
				case "id": {
					playerIds.put(tupleId, prop.getValue().toString());
					break;
				}
				case "source": {
					sourceIds.put(tupleId, prop.getValue().toString());
					break;
				}
				default: {
					throw new IllegalArgumentException(
							String.format("No logic for handling property \"%s\".", propName));
				}
				}
			}
		}

		final IntSet propCounts = new IntOpenHashSet(3);
		propCounts.add(playerIds.size());
		propCounts.add(sourceIds.size());
		propCounts.add(eventLogPaths.size());
		if (propCounts.size() > 1) {
			throw new IllegalArgumentException("Not all player data tuples are well-formed.");
		}

		final BiMap<String, String> playerSourceIds = HashBiMap.create(sourceIds.size());
		final Map<String, Path> playerEventLogPaths = Maps.newHashMapWithExpectedSize(eventLogPaths.size());
		for (final Entry<String, String> tuplePlayerId : playerIds.entrySet()) {
			final String tupleId = tuplePlayerId.getKey();
			final String playerId = tuplePlayerId.getValue();
			final String playerSourceId = sourceIds.get(tupleId);
			playerSourceIds.put(playerId, playerSourceId);
			final Path playerEventLogPath = eventLogPaths.get(tupleId);
			playerEventLogPaths.put(playerId, playerEventLogPath);
		}

		return new PlayerDataManager(playerSourceIds, playerEventLogPaths);
	}

	private final Map<String, Path> playerEventLogs;

	private final BiMap<String, String> playerSourceIds;

	public PlayerDataManager(final BiMap<String, String> playerSourceIds, final Map<String, Path> playerEventLogs) {
		this.playerSourceIds = Maps.unmodifiableBiMap(playerSourceIds);
		this.playerEventLogs = Collections.unmodifiableMap(playerEventLogs);
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
		if (!(obj instanceof PlayerDataManager)) {
			return false;
		}
		final PlayerDataManager other = (PlayerDataManager) obj;
		if (playerEventLogs == null) {
			if (other.playerEventLogs != null) {
				return false;
			}
		} else if (!playerEventLogs.equals(other.playerEventLogs)) {
			return false;
		}
		if (playerSourceIds == null) {
			if (other.playerSourceIds != null) {
				return false;
			}
		} else if (!playerSourceIds.equals(other.playerSourceIds)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the playerEventLogs
	 */
	public Map<String, Path> getPlayerEventLogs() {
		return playerEventLogs;
	}

	/**
	 * @return the playerSourceIds
	 */
	public BiMap<String, String> getPlayerSourceIds() {
		return playerSourceIds;
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
		result = prime * result + (playerEventLogs == null ? 0 : playerEventLogs.hashCode());
		result = prime * result + (playerSourceIds == null ? 0 : playerSourceIds.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(64 * playerEventLogs.size() + 1);
		builder.append("PlayerDataManager [playerEventLogs=");
		builder.append(playerEventLogs);
		builder.append(", playerSourceIds=");
		builder.append(playerSourceIds);
		builder.append(']');
		return builder.toString();
	}

}
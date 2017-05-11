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
import com.google.common.collect.Sets;

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

		final Set<Integer> propCounts = Sets.newHashSet(playerIds.size(), sourceIds.size(), eventLogPaths.size());
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
		this.playerSourceIds = playerSourceIds;
		this.playerEventLogs = playerEventLogs;
	}

	/**
	 * @return the playerEventLogs
	 */
	public Map<String, Path> getPlayerEventLogs() {
		return Collections.unmodifiableMap(playerEventLogs);
	}

	/**
	 * @return the playerSourceIds
	 */
	public BiMap<String, String> getPlayerSourceIds() {
		return Maps.unmodifiableBiMap(playerSourceIds);
	}

}
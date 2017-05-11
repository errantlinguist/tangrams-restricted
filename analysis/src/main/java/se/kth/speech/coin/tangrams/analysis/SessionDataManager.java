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
import java.util.Properties;

import se.kth.speech.io.RelativePaths;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 11 May 2017
 *
 */
public final class SessionDataManager {

	public static SessionDataManager create(final Properties props, final Path baseDir) {
		final Path hatInfilePath = RelativePaths.resolveIfNotAbsolute(Paths.get(props.getProperty("hat")), baseDir);
		final Path canonicalEventLogPath = RelativePaths
				.resolveIfNotAbsolute(Paths.get(props.getProperty("canonicalEvents")), baseDir);
		final PlayerDataManager playerData = PlayerDataManager.create(props, baseDir);
		return new SessionDataManager(hatInfilePath, canonicalEventLogPath, playerData);
	}

	private final Path canonicalEventLogPath;

	private final Path hatFilePath;

	private final PlayerDataManager playerData;

	private SessionDataManager(final Path hatFilePath, final Path canonicalEventLogPath,
			final PlayerDataManager playerData) {
		this.hatFilePath = hatFilePath;
		this.canonicalEventLogPath = canonicalEventLogPath;
		this.playerData = playerData;
	}

	/**
	 * @return the canonicalEventLogPath
	 */
	public Path getCanonicalEventLogPath() {
		return canonicalEventLogPath;
	}

	/**
	 * @return the hatFilePath
	 */
	public Path getHATFilePath() {
		return hatFilePath;
	}

	/**
	 * @return the playerData
	 */
	public PlayerDataManager getPlayerData() {
		return playerData;
	}

}

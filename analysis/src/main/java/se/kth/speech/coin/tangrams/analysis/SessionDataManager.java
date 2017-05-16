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

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.io.RelativePaths;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 11 May 2017
 *
 */
public final class SessionDataManager {

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionDataManager.class);

	public static SessionDataManager create(final Properties props, final Path baseDir) {
		final Path hatInfilePath = RelativePaths.resolveIfNotAbsolute(Paths.get(props.getProperty("hat")), baseDir);
		final Path canonicalEventLogPath = RelativePaths
				.resolveIfNotAbsolute(Paths.get(props.getProperty("canonicalEvents")), baseDir);
		final PlayerDataManager playerData = PlayerDataManager.create(props, baseDir);
		return new SessionDataManager(hatInfilePath, canonicalEventLogPath, playerData);
	}

	public static Map<Path, SessionDataManager> createFileSessionDataMap(final Iterable<Path> inpaths)
			throws IOException {
		final Map<Path, SessionDataManager> result = new HashMap<>();
		for (final Path inpath : inpaths) {
			LOGGER.info("Looking for session data underneath \"{}\".", inpath);
			final Path[] infiles = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
					.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
			for (final Path infile : infiles) {
				putSessionData(result, infile);
			}
		}
		return result;
	}

	private static void putSessionData(final Map<Path, SessionDataManager> fileSessionData, final Path infilePath)
			throws IOException {
		LOGGER.info("Reading session properties from \"{}\".", infilePath);
		final Properties props = new Properties();
		try (final InputStream propsInstream = Files.newInputStream(infilePath)) {
			props.load(propsInstream);
			final Path infileBaseDir = infilePath.getParent();
			final SessionDataManager sessionData = SessionDataManager.create(props, infileBaseDir);
			fileSessionData.put(infilePath, sessionData);
		}
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
		if (!(obj instanceof SessionDataManager)) {
			return false;
		}
		final SessionDataManager other = (SessionDataManager) obj;
		if (canonicalEventLogPath == null) {
			if (other.canonicalEventLogPath != null) {
				return false;
			}
		} else if (!canonicalEventLogPath.equals(other.canonicalEventLogPath)) {
			return false;
		}
		if (hatFilePath == null) {
			if (other.hatFilePath != null) {
				return false;
			}
		} else if (!hatFilePath.equals(other.hatFilePath)) {
			return false;
		}
		if (playerData == null) {
			if (other.playerData != null) {
				return false;
			}
		} else if (!playerData.equals(other.playerData)) {
			return false;
		}
		return true;
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (canonicalEventLogPath == null ? 0 : canonicalEventLogPath.hashCode());
		result = prime * result + (hatFilePath == null ? 0 : hatFilePath.hashCode());
		result = prime * result + (playerData == null ? 0 : playerData.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("SessionDataManager [canonicalEventLogPath=");
		builder.append(canonicalEventLogPath);
		builder.append(", hatFilePath=");
		builder.append(hatFilePath);
		builder.append(", playerData=");
		builder.append(playerData);
		builder.append("]");
		return builder.toString();
	}

}

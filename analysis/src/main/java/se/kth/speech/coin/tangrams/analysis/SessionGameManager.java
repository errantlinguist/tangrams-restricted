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
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 May 2017
 *
 */
public final class SessionGameManager {

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionGameManager.class);

	private final SessionGame canonicalGame;

	private final Map<String, Path> playerEventLogs;

	private final List<Utterance> utts;

	SessionGameManager(final SessionDataManager sessionData) throws IOException, JAXBException {
		utts = SessionUtterances.createUtteranceList(sessionData);
		LOGGER.debug("Creating dialogues for {} annotated utterance(s).", utts.size());

		canonicalGame = SessionGame.createGame(sessionData.getCanonicalEventLogPath(), utts);
		playerEventLogs = sessionData.getPlayerData().getPlayerEventLogs();
	}

	public Map<String, SessionGame> createPlayerPerspectiveGameMap() throws IOException {
		return SessionGame.createPlayerPerspectiveGameMap(playerEventLogs.entrySet(), utts);
	}

	public SessionGame getCanonicalGame() {
		return canonicalGame;
	}

}

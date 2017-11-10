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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import se.kth.speech.MapCollectors;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 26, 2017
 *
 */
final class TestSessionData {

	public static Map<SessionDataManager, Path> readTestSessionData(final Iterable<Path> inpaths) throws IOException {
		final Map<Path, SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths);
		final Map<SessionDataManager, Path> result = infileSessionData.entrySet().stream()
				.collect(Collectors.toMap(Entry::getValue, Entry::getKey, MapCollectors.throwingMerger(), () -> new HashMap<>(infileSessionData.size() + 1, 1.0f)));
		infileSessionData.forEach((infile, sessionData) -> result.put(sessionData, infile));
		return result;
	}

	private TestSessionData() {

	}

}

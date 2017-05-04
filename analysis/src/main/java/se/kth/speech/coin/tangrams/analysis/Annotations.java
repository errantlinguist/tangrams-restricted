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

import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources.Source;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 28 Apr 2017
 *
 */
public final class Annotations {

	private static final Logger LOGGER = LoggerFactory.getLogger(Annotations.class);

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4546093/1391325">StackOverflow</a>
	 */
	private static final Pattern MINIMAL_FILE_EXT_PATTERN = Pattern.compile("\\.(?=[^\\.]+$)");

	public static BiMap<String, String> createSourceIdPlayerIdMap(final Annotation uttAnnots) {
		final List<Track> tracks = uttAnnots.getTracks().getTrack();
		final List<Source> sources = tracks.stream().map(Track::getSources).map(Sources::getSource)
				.flatMap(List::stream).collect(Collectors.toList());
		final BiMap<String, String> result = HashBiMap.create(sources.size());
		for (final Source source : sources) {
			final String href = source.getHref();
			String fileName = null;
			try {
				final Path path = Paths.get(href);
				fileName = path.getFileName().toString();
			} catch (final InvalidPathException e) {
				LOGGER.debug(String.format(
						"Could not create path from \"%s\"; Trying to parse it directly as a filename.", href), e);
				fileName = href;
			}
			final String playerId = MINIMAL_FILE_EXT_PATTERN.split(fileName)[0];
			final String sourceId = source.getId();
			final String oldPlayerId = result.put(sourceId, playerId);
			assert oldPlayerId != null;
		}

		return result;
	}

	private Annotations() {
	}

}

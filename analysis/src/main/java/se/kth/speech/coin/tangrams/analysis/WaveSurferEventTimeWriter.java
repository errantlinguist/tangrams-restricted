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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import iristk.system.Event;
import iristk.util.Record.JsonToRecordException;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEventReader;

/**
 * Writes referent entity shapes for each game round to a format readable by <a href="https://sourceforge.net/projects/wavesurfer/">WaveSurfer</a>.
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 29 Aug 2017
 * @see <a href="https://sourceforge.net/projects/wavesurfer/">WaveSurfer website</a>
 *
 */
public final class WaveSurferEventTimeWriter { // NO_UCD (use default)

	private static final int ENTITY_ID_COL_IDX = 0;

	private static final long HEADER_ROW_COUNT = 2;

	private static final String IMG_INFO_COL_DELIMITER = "\t";

	private static final Pattern IMG_INFO_FILENAME_PATTERN = Pattern.compile(".*?img-info-(.+)\\.tsv");

	private static final Logger LOGGER = LoggerFactory.getLogger(WaveSurferEventTimeWriter.class);

	private static final Charset OUTPUT_ENCODING = StandardCharsets.UTF_8;

	private static final int SHAPE_COL_IDX = 1;

	private static final String TURN_DELIMITING_EVENT_NAME = GameManagementEvent.NEXT_TURN_REQUEST.getEventName();

	public static void main(final String[] args) throws IOException {
		final Path[] inpaths = Arrays.stream(args).map(Paths::get)
				.toArray(Path[]::new);
		if (inpaths.length < 1) {
			throw new IllegalArgumentException(
					String.format("Usage: %s INPATHS...", WaveSurferEventTimeWriter.class.getName()));
		} else {
			for (final Path inpath : inpaths) {
				final Iterable<Path> childDirs = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isDirectory)::iterator;
				for (final Path childDir : childDirs) {
					accept(childDir);
				}
			}
		}
	}

	private static void accept(final Path sessionDir) throws JsonToRecordException, IOException {
		final List<Path> files = Arrays.asList(Files.list(sessionDir).toArray(Path[]::new));
		final Map<String, Path> subjectImgInfoFiles = createSubjectImgInfoFileMap(files);
		if (subjectImgInfoFiles.isEmpty()) {
			LOGGER.debug("Input directory \"{}\" does not contain any image info files.", sessionDir);
		} else {
			for (final Entry<String, Path> subjectImgInfoFile : subjectImgInfoFiles.entrySet()) {
				final String subj = subjectImgInfoFile.getKey();
				final Path imgInfoFIle = subjectImgInfoFile.getValue();
				final Map<String, String> pieceShapes = readImgShapes(imgInfoFIle);

				final Path eventLogFilePath = sessionDir.resolve("events-" + subj + ".txt");
				final File outfile = new File(sessionDir.toFile(), subj + "-ws.txt");
				LOGGER.info("Writing to \"{}\".", outfile);
				try (PrintWriter outputWriter = new PrintWriter(
						new OutputStreamWriter(new FileOutputStream(outfile), OUTPUT_ENCODING))) {
					try (Stream<Event> events = LoggedEventReader.readLoggedEvents(eventLogFilePath)) {

						LocalTime startTime = null;
						String lastp = null;
						Double lastt = null;

						final Iterator<Event> eventIter = events.iterator();
						while (eventIter.hasNext()) {
							final Event event = eventIter.next();
							final LocalTime time = EventTimes.parseEventTime(event.getTime()).toLocalTime();
							if (event.has(GameManagementEvent.Attribute.GAME_STATE.toString()) && startTime == null) {
								startTime = time;
							}
							if (TURN_DELIMITING_EVENT_NAME.equals(event.getName())) {
								final double t = ChronoUnit.MILLIS.between(startTime, time) / 1000d;
								final String pid = event.getString("MOVE:pieceId");
								if (lastp != null) {
									outputWriter.println(lastt + " " + t + " " + lastp);
								}
								lastp = pieceShapes.get(pid);
								lastt = t;
							}
						}
					}
				}
			}
		}
	}

	private static Map<String, Path> createSubjectImgInfoFileMap(final Collection<Path> files) {
		final Map<String, Path> result = Maps.newHashMapWithExpectedSize(Math.min(files.size(), 2));
		for (final Path file : files) {
			final Path fileName = file.getFileName();
			assert fileName != null;
			final String filenameStr = fileName.toString();
			LOGGER.debug("Testing filename \"{}\".", filenameStr);
			final Matcher imgInfoFilenameMatcher = IMG_INFO_FILENAME_PATTERN.matcher(filenameStr);
			if (imgInfoFilenameMatcher.matches()) {
				final String subject = imgInfoFilenameMatcher.group(1);
				result.put(subject, file);
			}
		}
		return result;
	}

	private static Map<String, String> readImgShapes(final Path infile) throws IOException {
		final Map<String, String> result;
		try (Stream<String> lines = Files.lines(infile)) {
			final String[][] rows = lines.map(String::trim).filter(str -> !str.isEmpty())
					.map(str -> str.split(IMG_INFO_COL_DELIMITER)).skip(HEADER_ROW_COUNT).toArray(String[][]::new);
			result = Arrays.stream(rows)
					.collect(Collectors.toMap(row -> row[ENTITY_ID_COL_IDX], row -> row[SHAPE_COL_IDX]));

		}
		return result;
	}

	private WaveSurferEventTimeWriter() {
	}

}

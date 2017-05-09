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
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.xml.bind.JAXBException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.MissingOptionException;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Table;

import iristk.system.Event;
import iristk.util.HAT;
import se.kth.speech.awt.LookAndFeels;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.GameStateDescriptions;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.coin.tangrams.view.UserPrompts;
import se.kth.speech.hat.xsd.Annotation;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
public final class EventUtteranceTimeComparisonParamTester {

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(EventUtteranceTimeComparisonParamTester.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static class Settings {

		private final Properties props;

		private Settings(final Properties props) {
			this.props = props;
		}

		public Optional<String> getInpath() {
			final String propVal = props.getProperty("inpath");
			return propVal == null ? Optional.empty() : Optional.of(propVal);
		}

		public void setInpath(final String propVal) {
			props.setProperty("inpath", propVal);
		}

		private Properties getProperties() {
			return props;
		}
	}

	private static final Path CLASS_SETTINGS_INFILE_PATH;

	private static final FileNameExtensionFilter DEFAULT_FILE_FILTER;

	private static final List<FileNameExtensionFilter> FILE_FILTERS;

	private static final Logger LOGGER = LoggerFactory.getLogger(EventUtteranceTimeComparisonParamTester.class);

	private static final EventTypeMatcher SALIENT_PIECE_EVENT_MATCHER;

	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	private static final Path SETTINGS_DIR;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final Collector<CharSequence, ?, String> TABLE_ROW_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER;

	private static final Collector<CharSequence, ?, String> WORD_JOINER = Collectors.joining(" ");

	static {
		SALIENT_PIECE_EVENT_MATCHER = new EventTypeMatcher(EnumSet.of(GameManagementEvent.NEXT_TURN_REQUEST));
	}

	static {
		FILE_FILTERS = Arrays.asList(new FileNameExtensionFilter("Property files (*.properties)", "properties"));
		DEFAULT_FILE_FILTER = FILE_FILTERS.iterator().next();
	}

	static {
		TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();
		TABLE_ROW_JOINER = Collectors.joining(TABLE_STRING_REPR_ROW_DELIMITER);
	}

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}

	static {
		SETTINGS_DIR = Paths.get(".settings");
		CLASS_SETTINGS_INFILE_PATH = SETTINGS_DIR
				.resolve(EventUtteranceTimeComparisonParamTester.class.getName() + ".properties");
	}

	public static void main(final CommandLine cl) throws IOException, JAXBException, MissingOptionException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final Path[] inpaths = cl.getArgList().stream().map(Paths::get).toArray(Path[]::new);
			if (inpaths.length < 1) {
				throw new MissingOptionException("No input path(s) specified.");
			} else {
				for (final Path inpath : inpaths) {
					LOGGER.info("Will read batch job data from \"{}\".", inpath);
					run(inpath);
				}
			}
		}
	}

	public static void main(final String[] args) throws IOException, JAXBException {
		if (args.length < 1) {
			runInteractively();
		} else {
			final CommandLineParser parser = new DefaultParser();
			try {
				final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
				main(cl);
			} catch (final ParseException e) {
				System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
				Parameter.printHelp();
			}
		}

	}

	private static List<List<String>> createColHeaders() {
		// final List<List<String>> imgViewDescColHeaders =
		// ImageVisualizationInfoTableRowWriter.createColumnHeaders();
		// final int resultColCount =
		// imgViewDescColHeaders.stream().mapToInt(List::size).max().getAsInt()
		// + FEATURES_TO_DESCRIBE.size();
		//
		// final Iterator<List<String>> imgDescHeaderIter =
		// imgViewDescColHeaders.iterator();
		List<List<String>> result;
		// if (imgDescHeaderIter.hasNext()) {
		// result = new ArrayList<>(imgViewDescColHeaders.size());
		// final List<String> firstHeader = new ArrayList<>(resultColCount);
		// result.add(firstHeader);
		//
		// FEATURES_TO_DESCRIBE.stream().map(Object::toString).forEachOrdered(firstHeader::add);
		// final List<String> firstImgDescHeader = imgDescHeaderIter.next();
		// firstHeader.addAll(firstImgDescHeader);
		// final String padding = "";
		// while (firstHeader.size() < resultColCount) {
		// firstHeader.add(padding);
		// }
		//
		// while (imgDescHeaderIter.hasNext()) {
		// final List<String> nextImgDescHeader = imgDescHeaderIter.next();
		// final List<String> nextHeader = new ArrayList<>(resultColCount);
		// result.add(nextHeader);
		//
		// // Add padding for feature-derived descriptions
		// FEATURES_TO_DESCRIBE.stream().map(feature ->
		// padding).forEach(nextHeader::add);
		// nextHeader.addAll(nextImgDescHeader);
		// }
		//
		// } else {
		result = Collections.emptyList();
		// }
		return result;
	}

	private static Settings loadClassSettings() {
		final Properties settingsProps = new Properties();
		try {
			loadClassSettingsProps(settingsProps);
		} catch (final IOException e) {
			LOGGER.info(
					"A(n) {} occurred while trying to load the class settings from \"{}\"; Falling back to defaults.",
					e.getClass().getSimpleName(), CLASS_SETTINGS_INFILE_PATH);
		}
		return new Settings(settingsProps);
	}

	private static void loadClassSettingsProps(final Properties props) throws IOException {
		final Path classSettingsInfilePath = SETTINGS_DIR
				.resolve(EventUtteranceTimeComparisonParamTester.class.getName() + ".properties");
		Files.createDirectories(SETTINGS_DIR);
		try (InputStream classSettingsPropsInstream = Files.newInputStream(classSettingsInfilePath)) {
			props.load(classSettingsPropsInstream);
		}
	}

	private static void run(final Path inpath) throws JAXBException, IOException {
		final Iterator<Path> infilePathIter = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS)
				.filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).iterator();
		while (infilePathIter.hasNext()) {
			final Path infilePath = infilePathIter.next();
			LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
			final Properties props = new Properties();
			try (final InputStream propsInstream = Files.newInputStream(infilePath)) {
				props.load(propsInstream);
			}
			run(props, infilePath.getParent());
		}
	}

	private static void run(final Properties props, final Path infileBaseDir) throws JAXBException, IOException {
		final Path hatInfilePath = infileBaseDir.resolve(props.getProperty("hat"));
		LOGGER.info("Reading annotations from \"{}\".", hatInfilePath);
		final Annotation uttAnnots = HAT.readAnnotation(hatInfilePath.toFile());

		final PlayerDataManager playerData = PlayerDataManager.parsePlayerProps(props, infileBaseDir);
		final Table<String, String, GameHistory> playerGameHistoryTable = LoggedEvents.createPlayerGameHistoryTable(
				playerData.getPlayerEventLogs().entrySet(), LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);
		GameStateDescriptions.findAnyEquivalentGameState(
				playerGameHistoryTable.values().stream().map(GameHistory::getInitialState).iterator());

		final Map<Utterance, String> uttPlayerIds = new UtterancePlayerIdMapFactory(SEG_UTT_FACTORY::create,
				playerData.getPlayerSourceIds().inverse()::get).apply(uttAnnots.getSegments().getSegment());
		final List<Utterance> allUtts = Arrays
				.asList(uttPlayerIds.keySet().stream().sorted().toArray(Utterance[]::new));

		final List<List<String>> colHeaders = createColHeaders();
		final String headerStr = colHeaders.stream().map(header -> header.stream().collect(TABLE_ROW_CELL_JOINER))
				.collect(TABLE_ROW_JOINER);

		final EventUtteranceFactory eventUttFactory = new EventUtteranceFactory(SALIENT_PIECE_EVENT_MATCHER, 0);

		for (final Entry<String, Map<String, GameHistory>> playerGameHistories : playerGameHistoryTable.columnMap()
				.entrySet()) {
			final String playerId = playerGameHistories.getKey();
			LOGGER.info("Processing player \"{}\".", playerId);
			for (final Entry<String, GameHistory> gameHistory : playerGameHistories.getValue().entrySet()) {
				final String gameId = gameHistory.getKey();
				LOGGER.info("Processing game \"{}\".", gameId);
				final GameHistory history = gameHistory.getValue();
				final List<Entry<Event, List<Utterance>>> eventUttLists = eventUttFactory
						.apply(allUtts.listIterator(), history).collect(Collectors.toList());

				final Multimap<Utterance, Event> uttEvents = HashMultimap.create(eventUttLists.size(), 1);
				final List<Entry<Event, List<Utterance>>> eventWithUtts = new ArrayList<>(eventUttLists.size());
				for (final Entry<Event, List<Utterance>> eventUtts : eventUttLists) {
					final Event event = eventUtts.getKey();
					final List<Utterance> utts = eventUtts.getValue();
					if (utts.isEmpty()) {
						// LOGGER.warn("No utterances for event \"{}\".",
						// event);
					} else {
						eventWithUtts.add(eventUtts);
						utts.forEach(utt -> {
							uttEvents.put(utt, event);
						});
					}
				}

				System.out.println("Events with utts: " + eventWithUtts.size());

				final List<Utterance> overlappingUtts = new ArrayList<>();
				for (final Entry<Utterance, Collection<Event>> entry : uttEvents.asMap().entrySet()) {
					final Utterance utt = entry.getKey();
					final Collection<Event> events = entry.getValue();
					final int duplicateEventCount = events.size();
					if (duplicateEventCount > 0) {
						LOGGER.debug("Utt mapped to more than one event: \"{}\"",
								utt.getTokens().stream().collect(WORD_JOINER));
						overlappingUtts.add(utt);
					}
				}

				System.out.println("Number of overlapping utt: " + overlappingUtts.size());

			}
		}
	}

	private static void runInteractively() throws IOException {
		LookAndFeels.setLookAndFeel();
		final Settings settings = loadClassSettings();
		final File currentInpath = new File(settings.getInpath().orElse(System.getProperty("user.dir")));
		final JFileChooser fileChooser = new JFileChooser(currentInpath);
		FILE_FILTERS.stream().forEachOrdered(fileChooser::addChoosableFileFilter);
		fileChooser.setFileFilter(DEFAULT_FILE_FILTER);
		fileChooser.setDialogTitle("Input file");
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		UserPrompts.promptFile(fileChooser).map(File::toPath).ifPresent(inpath -> {
			LOGGER.info("Will read annotations from \"{}\".", inpath);
			settings.setInpath(inpath.toString());
			try {
				run(inpath);
			} catch (final JAXBException e) {
				throw new RuntimeException(e);
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
		});

		LOGGER.debug("Saving class settings to \"{}\".", CLASS_SETTINGS_INFILE_PATH);
		try (OutputStream settingsOutStream = Files.newOutputStream(CLASS_SETTINGS_INFILE_PATH,
				StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
			settings.getProperties().store(settingsOutStream, String.format("Persisted settings for class \"%s\".",
					EventUtteranceTimeComparisonParamTester.class.getName()));
		}
	}
}

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

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

import com.google.common.collect.Table;

import iristk.system.Event;
import iristk.util.HAT;
import se.kth.speech.FilenameBaseSplitter;
import se.kth.speech.awt.LookAndFeels;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature.Extractor;
import se.kth.speech.coin.tangrams.analysis.features.GameContextFeatureExtractor;
import se.kth.speech.coin.tangrams.analysis.features.ImageEdgeCounter;
import se.kth.speech.coin.tangrams.analysis.features.SelectedEntityFeatureExtractor;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableRowWriter;
import se.kth.speech.coin.tangrams.iristk.EventTimes;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.ImageVisualizationInfoUnmarshaller;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.coin.tangrams.view.UserPrompts;
import se.kth.speech.hat.xsd.Annotation;
import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
public final class UtteranceSelectedEntityDescriptionWriter {

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		OUTFILE_PREFIX("p") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outfile-prefix").desc("A prefix to add to the output files.")
						.hasArg().argName("prefix").build();
			}
		},
		OUTPATH("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path to write the output to.").hasArg()
						.argName("path").type(File.class).required().build();
			}
		},
		STRICT("s") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("strict")
						.desc("Causes the extraction to fail if at least one utterance for each event wasn't found.")
						.build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static String parseOutfilePrefix(final CommandLine cl, final Path inpath) {
			final String prefix = cl.getOptionValue(Parameter.OUTFILE_PREFIX.optName, DEFAULT_OUTFILE_PREFIX);
			return prefix;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(UtteranceSelectedEntityDescriptionWriter.class.getSimpleName() + " INPATHS...",
					OPTIONS);
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

		public Optional<String> getOutpath() {
			final String propVal = props.getProperty("outpath");
			return propVal == null ? Optional.empty() : Optional.of(propVal);
		}

		public void setInpath(final String propVal) {
			props.setProperty("inpath", propVal);
		}

		public void setOutpath(final String propVal) {
			props.setProperty("outpath", propVal);
		}

		private Properties getProperties() {
			return props;
		}
	}

	private static class TabularDataWriter {

		private static final String BLANK_IMG_DESC;

		private static final EventUtteranceFactory EVENT_UTT_FACTORY = new EventUtteranceFactory(
				new EventTypeMatcher(EnumSet.of(GameManagementEvent.NEXT_TURN_REQUEST)));

		private static final String HEADER_STR;

		private static final ImageVisualizationInfoUnmarshaller IMG_VIZ_INFO_UNMARSHALLER = new ImageVisualizationInfoUnmarshaller();

		private static final Collector<CharSequence, ?, String> SENTENCE_JOINER = Collectors.joining(". ");

		private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

		private static final Collector<CharSequence, ?, String> TABLE_ROW_JOINER;

		private static final String TABLE_STRING_REPR_COL_DELIMITER;

		private static final String TABLE_STRING_REPR_ROW_DELIMITER;

		private static final Collector<CharSequence, ?, String> WORD_JOINER = Collectors.joining(" ");

		static {
			TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();
			TABLE_ROW_JOINER = Collectors.joining(TABLE_STRING_REPR_ROW_DELIMITER);
		}

		static {
			TABLE_STRING_REPR_COL_DELIMITER = "\t";
			TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
		}

		static {
			final List<List<String>> colHeaders = createColHeaders();
			HEADER_STR = colHeaders.stream().map(header -> header.stream().collect(TABLE_ROW_CELL_JOINER))
					.collect(TABLE_ROW_JOINER);
			BLANK_IMG_DESC = createBlankImgDesc(colHeaders);
		}

		private static String createBlankImgDesc(final List<List<String>> colHeaders) {
			final int colCount = colHeaders.stream().mapToInt(List::size).max().getAsInt();
			final String[] blankCells = new String[colCount];
			Arrays.fill(blankCells, "-");
			return Arrays.stream(blankCells).collect(TABLE_ROW_CELL_JOINER);
		}

		private static List<List<String>> createColHeaders() {
			final List<List<String>> imgViewDescColHeaders = ImageVisualizationInfoTableRowWriter.createColumnHeaders();
			final int resultColCount = imgViewDescColHeaders.stream().mapToInt(List::size).max().getAsInt()
					+ FEATURES_TO_DESCRIBE.size() + 1;

			final Iterator<List<String>> imgDescHeaderIter = imgViewDescColHeaders.iterator();
			List<List<String>> result;
			if (imgDescHeaderIter.hasNext()) {
				result = new ArrayList<>(imgViewDescColHeaders.size());
				final List<String> firstHeader = new ArrayList<>(resultColCount);
				result.add(firstHeader);

				firstHeader.add("TIME");
				FEATURES_TO_DESCRIBE.stream().map(Object::toString).forEachOrdered(firstHeader::add);
				final List<String> firstImgDescHeader = imgDescHeaderIter.next();
				firstHeader.addAll(firstImgDescHeader);
				final String padding = "";
				while (firstHeader.size() < resultColCount) {
					firstHeader.add(padding);
				}

				// Add subheader for image description-specific features, e.g.
				// color
				// features
				while (imgDescHeaderIter.hasNext()) {
					final List<String> nextImgDescHeader = imgDescHeaderIter.next();
					final List<String> nextHeader = new ArrayList<>(resultColCount);
					result.add(nextHeader);

					// Add padding for timestamp col
					nextHeader.add(padding);
					// Add padding for feature-derived descriptions
					FEATURES_TO_DESCRIBE.stream().map(feature -> padding).forEach(nextHeader::add);
					nextHeader.addAll(nextImgDescHeader);
				}

			} else {
				result = Collections.emptyList();
			}
			return result;
		}

		private final GameContextFeatureExtractor ctxFeatureExtractor;

		private final boolean strict;

		private final TemporalGameContextFactory uttContextFactory;

		private final Function<? super Utterance, String> uttPlayerIdGetter;

		private final List<Utterance> utts;

		private TabularDataWriter(final List<Utterance> utts,
				final Function<? super Utterance, String> uttPlayerIdGetter,
				final TemporalGameContextFactory uttContextFactory,
				final GameContextFeatureExtractor ctxFeatureExtractor, final boolean strict) {
			this.utts = utts;
			this.uttPlayerIdGetter = uttPlayerIdGetter;
			this.uttContextFactory = uttContextFactory;
			this.ctxFeatureExtractor = ctxFeatureExtractor;
			this.strict = strict;
		}

		private String createNoEventUtterancesMsg(final Event event,
				final List<Entry<Event, List<Utterance>>> eventUttLists, final int eventIdx) {
			final StringBuilder sb = new StringBuilder(128);
			sb.append("No utterances for event index ");
			sb.append(eventIdx);
			sb.append(" \"");
			sb.append(event);
			sb.append("\".");
			{
				final ListIterator<Entry<Event, List<Utterance>>> eventUttListIter = eventUttLists
						.listIterator(eventIdx);
				Entry<Event, List<Utterance>> prevEventUttList = null;
				while (eventUttListIter.hasPrevious()) {
					prevEventUttList = eventUttListIter.previous();
					final List<Utterance> prevUtts = prevEventUttList.getValue();
					if (!prevUtts.isEmpty()) {
						break;
					}
				}
				if (prevEventUttList != null) {
					sb.append(System.lineSeparator());
					final Event prevEvent = prevEventUttList.getKey();
					final List<Utterance> prevUtts = prevEventUttList.getValue();
					final Utterance prevUtt = prevUtts.get(prevUtts.size() - 1);
					final String speakingPlayerId = uttPlayerIdGetter.apply(prevUtt);
					sb.append(String.format(
							"Last utt before event: \"%s\"; speaking player ID: \"%s\"; start: %f; end: %f; segment ID: \"%s\"; event ID: \"%s\"; event time: \"%s\"",
							prevUtt.getTokens().stream().collect(WORD_JOINER), speakingPlayerId, prevUtt.getStartTime(),
							prevUtt.getEndTime(), prevUtt.getSegmentId(), prevEvent.getId(), prevEvent.getTime()));
				}
			}
			{
				final ListIterator<Entry<Event, List<Utterance>>> eventUttListIter = eventUttLists
						.listIterator(eventIdx + 1);
				Entry<Event, List<Utterance>> nextEventUttList = null;
				while (eventUttListIter.hasNext()) {
					nextEventUttList = eventUttListIter.next();
					final List<Utterance> nextUtts = nextEventUttList.getValue();
					if (!nextUtts.isEmpty()) {
						break;
					}
				}
				if (nextEventUttList != null) {
					sb.append(System.lineSeparator());
					final Event nextEvent = nextEventUttList.getKey();
					final List<Utterance> nextUtts = nextEventUttList.getValue();
					final Utterance nextUtt = nextUtts.get(0);
					final String speakingPlayerId = uttPlayerIdGetter.apply(nextUtt);
					sb.append(String.format(
							"Next utt after event: \"%s\"; speaking player ID: \"%s\"; start: %f; end: %f; segment ID: \"%s\"; event ID: \"%s\"; event time: \"%s\"",
							nextUtt.getTokens().stream().collect(WORD_JOINER), speakingPlayerId, nextUtt.getStartTime(),
							nextUtt.getEndTime(), nextUtt.getSegmentId(), nextEvent.getId(), nextEvent.getTime()));
				}
			}
			return sb.toString();
		}

		private String createUtteranceDialogString(final Stream<Utterance> utts) {
			final Stream<String> uttStrs = utts.map(utt -> {
				final String playerId = uttPlayerIdGetter.apply(utt);
				return "**" + playerId + ":** \"" + utt.getTokens().stream().collect(WORD_JOINER) + "\"";
			});
			return uttStrs.collect(SENTENCE_JOINER);
		}

		void write(final String playerId, final GameHistory history, final Writer writer, final Instances instances)
				throws IOException {
			// The visualization info for the given game
			final ImageVisualizationInfo imgVizInfo = IMG_VIZ_INFO_UNMARSHALLER
					.apply(history.getInitialState().getImageVisualizationInfoDescription());
			final List<Entry<Event, List<Utterance>>> eventUttLists = EVENT_UTT_FACTORY
					.apply(utts.listIterator(), history).collect(Collectors.toList());

			writer.write(HEADER_STR);
			for (final ListIterator<Entry<Event, List<Utterance>>> eventUttListIter = eventUttLists
					.listIterator(); eventUttListIter.hasNext();) {
				final Entry<Event, List<Utterance>> eventUttList = eventUttListIter.next();
				writer.write(TABLE_STRING_REPR_ROW_DELIMITER);

				final Event event = eventUttList.getKey();
				final List<Utterance> eventUtts = eventUttList.getValue();

				final String imgVizInfoDesc;
				if (event == null) {
					imgVizInfoDesc = BLANK_IMG_DESC;
				} else {
					final StringWriter strWriter = new StringWriter(256);

					final double contextStartTime;
					final double contextEndTime;
					if (eventUtts.isEmpty()) {
						if (strict) {
							throw new IllegalArgumentException(String.format("No utterances for event \"%s\".", event));
						} else {
							final String msg = createNoEventUtterancesMsg(event, eventUttLists,
									eventUttListIter.nextIndex() - 1);
							LOGGER.warn(msg);
							final LocalDateTime eventTime = EventTimes.parseEventTime(event.getTime());
							final Duration gameDuration = Duration.between(history.getStartTime(), eventTime);
							final float offset = gameDuration.toMillis() / 1000.0f;
							contextStartTime = offset;
							contextEndTime = offset;
						}
					} else {
						// Just use the context of the first utterance
						final Utterance firstUtt = eventUtts.iterator().next();
						contextStartTime = firstUtt.getStartTime();
						contextEndTime = firstUtt.getEndTime();
					}
					writer.write(event.getTime());
					writer.write(TABLE_STRING_REPR_COL_DELIMITER);
					{
						final GameContext context = uttContextFactory.apply(contextStartTime, contextEndTime, playerId)
								.findFirst().get();
						final Instance inst = new DenseInstance(instances.numAttributes());
						inst.setDataset(instances);
						ctxFeatureExtractor.accept(context, inst);
						instances.add(inst);

						final double[] featureVector = inst.toDoubleArray();
						writer.write(
								Arrays.stream(featureVector).mapToObj(Double::toString).collect(TABLE_ROW_CELL_JOINER));
					}
					writer.write(TABLE_STRING_REPR_COL_DELIMITER);
					{
						final ImageVisualizationInfoTableRowWriter imgInfoDescWriter = new ImageVisualizationInfoTableRowWriter(
								strWriter);
						final Move move = (Move) event.get(GameManagementEvent.Attribute.MOVE.toString());
						final Integer selectedPieceId = move.getPieceId();
						final ImageVisualizationInfo.Datum selectedPieceImgVizInfo = imgVizInfo.getData()
								.get(selectedPieceId);
						imgInfoDescWriter.write(selectedPieceId, selectedPieceImgVizInfo);
					}

					imgVizInfoDesc = strWriter.toString();
				}
				writer.write(imgVizInfoDesc);
				writer.write(TABLE_STRING_REPR_COL_DELIMITER);

				final String eventDialogStr = createUtteranceDialogString(eventUtts.stream());
				writer.write(eventDialogStr);
			}
		}
	}

	private static final ArrayList<Attribute> ATTRS;

	private static final Path CLASS_SETTINGS_INFILE_PATH;

	private static final FileNameExtensionFilter DEFAULT_FILE_FILTER;

	private static final String DEFAULT_OUTFILE_PREFIX = "uttImgDescs_";

	private static final Extractor EXTRACTOR;

	private static final Map<EntityFeature, Attribute> FEATURE_ATTRS;

	private static final List<EntityFeature> FEATURES_TO_DESCRIBE;

	private static final List<FileNameExtensionFilter> FILE_FILTERS;

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceSelectedEntityDescriptionWriter.class);

	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	private static final Path SETTINGS_DIR;

	static {
		FILE_FILTERS = Arrays.asList(new FileNameExtensionFilter("Property files (*.properties)", "properties"));
		DEFAULT_FILE_FILTER = FILE_FILTERS.iterator().next();
	}

	static {
		SETTINGS_DIR = Paths.get(".settings");
		CLASS_SETTINGS_INFILE_PATH = SETTINGS_DIR
				.resolve(UtteranceSelectedEntityDescriptionWriter.class.getName() + ".properties");
	}

	static {
		FEATURES_TO_DESCRIBE = Arrays.asList(EntityFeature.POSITION_X, EntityFeature.POSITION_Y,
				EntityFeature.EDGE_COUNT);
		FEATURE_ATTRS = EntityFeature.Extractor.createFeatureAttrMap(FEATURES_TO_DESCRIBE);
		EXTRACTOR = new EntityFeature.Extractor(FEATURE_ATTRS);
		ATTRS = new ArrayList<>(FEATURE_ATTRS.size());
		ATTRS.addAll(FEATURE_ATTRS.values());
	}

	public static void main(final CommandLine cl) throws IOException, JAXBException, ParseException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final Path[] inpaths = cl.getArgList().stream().map(Paths::get).toArray(Path[]::new);
			if (inpaths.length < 1) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final Path outpath = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath();
				LOGGER.info("Will write data to \"{}\".", outpath);
				final boolean strict = cl.hasOption(Parameter.STRICT.optName);
				for (final Path inpath : inpaths) {
					final String outfileNamePrefix = Parameter.parseOutfilePrefix(cl, inpath);
					LOGGER.info("Will prefix each output file for input \"{}\" with \"{}\".", inpath,
							outfileNamePrefix);
					final UtteranceSelectedEntityDescriptionWriter writer = new UtteranceSelectedEntityDescriptionWriter(
							outpath, outfileNamePrefix, strict);
					LOGGER.info("Will read batch job data from \"{}\".", inpath);
					writer.accept(inpath);
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
				System.out.println(String.format("An error occurred while parsing the command-line arguments: %s", e));
				Parameter.printHelp();
			}
		}

	}

	private static String createOutfileInfix(final Path inpath) {
		final String parentDirName = inpath.getParent().getFileName().toString();
		final String fileBaseName = new FilenameBaseSplitter().apply(inpath.getFileName().toString())[0];
		return parentDirName + "-" + fileBaseName;
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
				.resolve(UtteranceSelectedEntityDescriptionWriter.class.getName() + ".properties");
		Files.createDirectories(SETTINGS_DIR);
		try (InputStream classSettingsPropsInstream = Files.newInputStream(classSettingsInfilePath)) {
			props.load(classSettingsPropsInstream);
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
			fileChooser.setDialogTitle("Output dir");
			fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			FILE_FILTERS.stream().forEachOrdered(fileChooser::removeChoosableFileFilter);
			settings.setInpath(inpath.toString());
			settings.getOutpath().map(File::new).ifPresent(fileChooser::setCurrentDirectory);
			UserPrompts.promptFile(fileChooser).map(File::toPath).ifPresent(outpath -> {
				LOGGER.info("Will write data to \"{}\".", outpath);
				settings.setOutpath(outpath.toString());
				UserPrompts.promptNonBlankString("Enter output filename prefix.", DEFAULT_OUTFILE_PREFIX)
						.ifPresent(outfileNamePrefix -> {
							LOGGER.info("Will prefix each output file with \"{}\".", outfileNamePrefix);
							try {
								new UtteranceSelectedEntityDescriptionWriter(outpath, outfileNamePrefix, false)
										.accept(inpath);
							} catch (final JAXBException e) {
								throw new RuntimeException(e);
							} catch (final IOException e) {
								throw new UncheckedIOException(e);
							}
						});
			});
		});

		LOGGER.debug("Saving class settings to \"{}\".", CLASS_SETTINGS_INFILE_PATH);
		try (OutputStream settingsOutStream = Files.newOutputStream(CLASS_SETTINGS_INFILE_PATH,
				StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
			settings.getProperties().store(settingsOutStream, String.format("Persisted settings for class \"%s\".",
					UtteranceSelectedEntityDescriptionWriter.class.getName()));
		}

	}

	private final String outfileNamePrefix;

	private final Path outpath;

	private final boolean strict;

	public UtteranceSelectedEntityDescriptionWriter(final Path outpath, final String outfileNamePrefix,
			final boolean strict) {
		this.outpath = outpath;
		this.outfileNamePrefix = outfileNamePrefix;
		this.strict = strict;
	}

	public void accept(final Path inpath) throws JAXBException, IOException {
		final Path[] infilePaths = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
		for (final Path infilePath : infilePaths) {
			LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
			final Properties props = new Properties();
			try (final InputStream propsInstream = Files.newInputStream(infilePath)) {
				props.load(propsInstream);
			}
			final String outfileInfix = createOutfileInfix(infilePath);
			accept(props, infilePath.getParent(), outfileNamePrefix + outfileInfix);
		}
	}

	private void accept(final Properties props, final Path infileBaseDir, final String outfileNamePrefix)
			throws JAXBException, IOException {
		final Path hatInfilePath = infileBaseDir.resolve(props.getProperty("hat"));
		LOGGER.info("Reading annotations from \"{}\".", hatInfilePath);
		final Annotation uttAnnots = HAT.readAnnotation(hatInfilePath.toFile());

		final PlayerDataManager playerData = PlayerDataManager.parsePlayerProps(props, infileBaseDir);

		final Table<String, String, GameHistory> gamePlayerHistoryTable = LoggedEvents.createPlayerGameHistoryTable(
				playerData.getPlayerEventLogs().entrySet(), LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);
		final Set<String> playerGameIdIntersection = new HashSet<>(gamePlayerHistoryTable.rowKeySet());
		gamePlayerHistoryTable.columnMap().values().stream().map(Map::keySet)
				.forEach(playerGameIdIntersection::retainAll);
		final int uniqueModelDescriptionCount = gamePlayerHistoryTable.values().size();
		final SelectedEntityFeatureExtractor entityFeatureExtractor = new SelectedEntityFeatureExtractor(EXTRACTOR,
				new GameContextModelFactory(uniqueModelDescriptionCount), new ImageEdgeCounter());

		final Map<Utterance, String> uttPlayerIds = new UtterancePlayerIdMapFactory(SEG_UTT_FACTORY::create,
				playerData.getPlayerSourceIds().inverse()::get).apply(uttAnnots.getSegments().getSegment());
		final List<Utterance> utts = Arrays.asList(uttPlayerIds.keySet().stream().sorted().toArray(Utterance[]::new));
		for (final String gameId : playerGameIdIntersection) {
			LOGGER.debug("Processing game \"{}\".", gameId);
			final Map<String, GameHistory> playerHistories = gamePlayerHistoryTable.row(gameId);
			final TemporalGameContextFactory uttContextFactory = new TemporalGameContextFactory(playerHistories::get);

			final Instances instances = new Instances("word_training", ATTRS, gamePlayerHistoryTable.size() * 1000);

			final TabularDataWriter gameWriter = new TabularDataWriter(utts, uttPlayerIds::get, uttContextFactory,
					entityFeatureExtractor, strict);
			for (final Entry<String, GameHistory> playerHistory : playerHistories.entrySet()) {
				final String playerId = playerHistory.getKey();
				final GameHistory history = playerHistory.getValue();
				final Path outfilePath = outpath
						.resolve(outfileNamePrefix + "_GAME-" + gameId + "_LOG-" + playerId + ".txt");
				LOGGER.info("Writing utterances from perspective of \"{}\" to \"{}\".", playerId, outfilePath);
				try (BufferedWriter writer = Files.newBufferedWriter(outfilePath, StandardOpenOption.CREATE,
						StandardOpenOption.TRUNCATE_EXISTING)) {
					gameWriter.write(playerId, history, writer, instances);
				}
			}
		}
	}
}

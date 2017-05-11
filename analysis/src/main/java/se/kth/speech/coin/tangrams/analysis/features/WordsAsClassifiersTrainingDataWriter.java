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
package se.kth.speech.coin.tangrams.analysis.features;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.Collator;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableSet;
import java.util.Properties;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

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

import com.google.common.collect.BiMap;
import com.google.common.collect.Maps;

import iristk.util.HAT;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameContextModelFactory;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SegmentUtteranceFactory;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContexts;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.UtterancePlayerIdMapFactory;
import se.kth.speech.coin.tangrams.analysis.vocab.WordLists;
import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.AbstractFileSaver;
import weka.core.converters.ConverterUtils;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see <a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.
 *
 */
public final class WordsAsClassifiersTrainingDataWriter {

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		OUTPATH("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path to write the training data to.")
						.hasArg().argName("path").type(File.class).build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static File parseOutpath(final CommandLine cl) throws ParseException {
			final File result;
			final File val = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
			if (val != null && val.isDirectory()) {
				// TODO: Find common path root for all input paths and use it to
				// make the default filename
				final String timestamp = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss").format(LocalDateTime.now());
				result = new File(val, "wordsAsClassifiers-" + timestamp + "." + DEFAULT_OUTFILE_EXT);
				LOGGER.warn("Supplied outpath \"{}\" is a directory; Using default output file path \"{}\".", val,
						result);
			} else {
				result = val;
			}
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(WordsAsClassifiersTrainingDataWriter.class.getSimpleName() + " INFILE", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final String DEFAULT_OUTFILE_EXT = "arff";

	private static final EntityFeature.Extractor EXTRACTOR;

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersTrainingDataWriter.class);

	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	static {
		final List<String> shapeFeatureVals = new ArrayList<>(IconImages.getImageResources().keySet());
		shapeFeatureVals.sort(Comparator.naturalOrder());
		EXTRACTOR = new EntityFeature.Extractor(shapeFeatureVals);
	}

	public static void main(final CommandLine cl) throws IOException, JAXBException, ParseException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final Path[] inpaths = cl.getArgList().stream().map(Paths::get).toArray(Path[]::new);
			if (inpaths.length < 1) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final AbstractFileSaver saver;
				final File outpath = Parameter.parseOutpath(cl);
				if (outpath == null) {
					LOGGER.info("Will write data to standard output stream.");
					saver = ConverterUtils.getSaverForExtension(DEFAULT_OUTFILE_EXT);
				} else {
					LOGGER.info("Will write data to \"{}\".", outpath);
					saver = ConverterUtils.getSaverForFile(outpath);
					saver.setFile(outpath);
				}

				final WordsAsClassifiersTrainingDataWriter writer = new WordsAsClassifiersTrainingDataWriter(saver);
				for (final Path inpath : inpaths) {
					LOGGER.info("Reading batch job data from \"{}\".", inpath);
					writer.accept(inpath);
				}
			}

		}
	}

	public static void main(final String[] args) throws IOException, JAXBException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static void accept(final SessionDataManager sessionData, final Instances instances)
			throws JAXBException, IOException {
		final Path hatInfilePath = sessionData.getHATFilePath();
		LOGGER.info("Reading annotations from \"{}\".", hatInfilePath);
		final Annotation uttAnnots = HAT.readAnnotation(hatInfilePath.toFile());

		final Path eventLogPath = sessionData.getCanonicalEventLogPath();
		LOGGER.info("Reading events from \"{}\".", eventLogPath);
		final Map<String, GameHistory> gameHistories = LoggedEvents.parseGameHistories(Files.lines(eventLogPath),
				LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);
		final int uniqueModelDescriptionCount = gameHistories.values().size();
		final List<GameContextFeatureExtractor> contextFeatureExtractors = Arrays
				.asList(new SelectedEntityFeatureExtractor(EXTRACTOR,
						new GameContextModelFactory(uniqueModelDescriptionCount), new ImageEdgeCounter()));

		final BiMap<String, String> playerSourceIds = sessionData.getPlayerData().getPlayerSourceIds();
		final Function<String, String> sourcePlayerIdGetter = playerSourceIds.inverse()::get;
		final Map<Utterance, String> uttPlayerIds = new UtterancePlayerIdMapFactory(SEG_UTT_FACTORY::create,
				sourcePlayerIdGetter).apply(uttAnnots.getSegments().getSegment());
		final List<Utterance> utts = Arrays.asList(uttPlayerIds.keySet().stream().sorted().toArray(Utterance[]::new));

		for (final Entry<String, GameHistory> gameHistory : gameHistories.entrySet()) {
			final String gameId = gameHistory.getKey();
			LOGGER.debug("Processing game \"{}\".", gameId);
			final GameHistory history = gameHistory.getValue();
			for (final String perspectivePlayerId : playerSourceIds.keySet()) {
				LOGGER.info("Processing game from perspective of player \"{}\".", perspectivePlayerId);

				utts.forEach(utt -> {
					final String uttPlayerId = uttPlayerIds.get(utt);
					if (perspectivePlayerId.equals(uttPlayerId)) {
						final Stream<Instance> uttInstances = createContextInstances(utt, contextFeatureExtractors,
								history, perspectivePlayerId, instances);
						uttInstances.forEachOrdered(uttInstance -> {
							final double[] ctxFeatures = uttInstance.toDoubleArray();
							utt.getTokens().forEach(token -> {
								final Instance tokenInst = uttInstance.copy(ctxFeatures);
								tokenInst.setDataset(instances);
								tokenInst.setClassValue(token);
								instances.add(tokenInst);
							});
						});
					} else {
						LOGGER.debug(
								"Skipping the extraction of features for utterance with segment ID \"{}\" because the utterance is from player \"{}\" rather than the player whose perspective is being used for extracting features (\"{}\")",
								utt.getSegmentId(), uttPlayerId, perspectivePlayerId);
					}

				});
			}
		}
	}

	private static Stream<Instance> createContextInstances(final Utterance utt,
			final List<GameContextFeatureExtractor> contextFeatureExtractors, final GameHistory history,
			final String perspectivePlayerId, final Instances instances) {
		final Stream<GameContext> uttContexts = TemporalGameContexts.create(history, utt.getStartTime(),
				utt.getEndTime(), perspectivePlayerId);
		return uttContexts.map(uttContext -> {
			final Instance inst = new DenseInstance(instances.numAttributes());
			inst.setDataset(instances);
			contextFeatureExtractors.forEach(extractor -> extractor.accept(uttContext, inst));
			return inst;
		});
	}

	private static Map<Path, SessionDataManager> createFileSessionDataMap(final Path[] infilePaths) throws IOException {
		final Map<Path, SessionDataManager> result = Maps.newHashMapWithExpectedSize(infilePaths.length);
		for (final Path infilePath : infilePaths) {
			LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
			final Properties props = new Properties();
			try (final InputStream propsInstream = Files.newInputStream(infilePath)) {
				props.load(propsInstream);
				final Path infileBaseDir = infilePath.getParent();
				final SessionDataManager sessionData = SessionDataManager.create(props, infileBaseDir);
				result.put(infilePath, sessionData);
			}
		}
		return result;
	}

	private static NavigableSet<String> createUnigramVocabulary(final Iterable<SessionDataManager> sessionData)
			throws JAXBException {
		final NavigableSet<String> result = new TreeSet<>(Collator.getInstance(WordLists.DEFAULT_LOCALE));
		for (final SessionDataManager sessionDatum : sessionData) {
			final Path hatFilePath = sessionDatum.getHATFilePath();
			LOGGER.info("Processing vocabulary data from \"{}\".", hatFilePath);
			final Annotation annot = HAT.readAnnotation(hatFilePath.toFile());
			final Stream<Segment> segs = annot.getSegments().getSegment().stream();
			final Stream<Utterance> utts = SEG_UTT_FACTORY.create(segs).map(List::stream).flatMap(Function.identity());
			utts.forEach(utt -> {
				final List<String> tokens = utt.getTokens();
				assert !tokens.isEmpty();
				result.addAll(tokens);
			});
		}
		return result;
	}

	private final AbstractFileSaver saver;

	public WordsAsClassifiersTrainingDataWriter(final AbstractFileSaver saver) {
		this.saver = saver;
	}

	public void accept(final Path inpath) throws JAXBException, IOException {
		final Map<Path, SessionDataManager> infileSessionData = createFileSessionDataMap(Files
				.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new));

		final List<String> classifications = new ArrayList<>(createUnigramVocabulary(infileSessionData.values()));
		final Map<EntityFeature, Attribute> featureAttrs = EXTRACTOR.getFeatureAttrs();
		final ArrayList<Attribute> attrs = new ArrayList<>(featureAttrs.size() + 1);
		attrs.addAll(featureAttrs.values());
		final Attribute classifictionAttr = new Attribute("WORD", classifications);
		attrs.add(classifictionAttr);
		final Instances instances = new Instances("word_training", attrs, infileSessionData.size() * 5000);
		instances.setClass(classifictionAttr);
		saver.setInstances(instances);

		for (final Entry<Path, SessionDataManager> infileSessionDataEntry : infileSessionData.entrySet()) {
			accept(infileSessionDataEntry.getValue(), instances);
		}
		LOGGER.info("Processed {} data point(s), with a total of {} distinct class value(s).", instances.numInstances(),
				instances.numDistinctValues(classifictionAttr));

		saver.writeBatch();
	}

}

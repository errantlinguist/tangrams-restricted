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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.Random;
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
import se.kth.speech.coin.tangrams.analysis.RandomNotSelectedEntityIdGetter;
import se.kth.speech.coin.tangrams.analysis.SegmentUtteranceFactory;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContexts;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.UtterancePlayerIdMapFactory;
import se.kth.speech.coin.tangrams.content.IconImages;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;
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

	private static class MultiClassDataCollector {

		private static final ImageEdgeCounter IMG_EDGE_COUNTER = new ImageEdgeCounter();

		private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

		private static final Function<GameContext, Optional<Integer>> SELECTED_ENTITY_ID_GETTER = ctx -> ctx
				.findLastSelectedEntityId();

		private final Function<? super String, Instances> classInstanceGetter;

		private final Function<GameContext, Integer> negativeExampleEntityIdGetter;

		private final int numAttributes;

		private MultiClassDataCollector(final Function<? super String, Instances> classInstanceGetter,
				final int numAttributes, final Function<GameContext, Integer> negativeExampleEntityIdGetter) {
			this.negativeExampleEntityIdGetter = negativeExampleEntityIdGetter;
			this.classInstanceGetter = classInstanceGetter;
			this.numAttributes = numAttributes;
		}

		private void accept(final SessionDataManager sessionData) throws JAXBException, IOException {
			final Path hatInfilePath = sessionData.getHATFilePath();
			LOGGER.info("Reading annotations from \"{}\".", hatInfilePath);
			final Annotation uttAnnots = HAT.readAnnotation(hatInfilePath.toFile());

			final Path eventLogPath = sessionData.getCanonicalEventLogPath();
			LOGGER.info("Reading events from \"{}\".", eventLogPath);
			final Map<String, GameHistory> gameHistories = LoggedEvents.parseGameHistories(Files.lines(eventLogPath),
					LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);
			final int uniqueModelDescriptionCount = gameHistories.values().size();

			final GameContextModelFactory modelFactory = new GameContextModelFactory(uniqueModelDescriptionCount);
			final GameContextFeatureExtractor positiveCtxFeatureExtractor = new SelectedEntityFeatureExtractor(
					EXTRACTOR, SELECTED_ENTITY_ID_GETTER.andThen(Optional::get), modelFactory, IMG_EDGE_COUNTER);

			final GameContextFeatureExtractor negativeCtxFeatureExtractor = new SelectedEntityFeatureExtractor(
					EXTRACTOR, negativeExampleEntityIdGetter, modelFactory, IMG_EDGE_COUNTER);

			final BiMap<String, String> playerSourceIds = sessionData.getPlayerData().getPlayerSourceIds();
			final Function<String, String> sourcePlayerIdGetter = playerSourceIds.inverse()::get;
			final Map<Utterance, String> uttPlayerIds = new UtterancePlayerIdMapFactory(SEG_UTT_FACTORY::create,
					sourcePlayerIdGetter).apply(uttAnnots.getSegments().getSegment());
			final List<Utterance> utts = Arrays
					.asList(uttPlayerIds.keySet().stream().sorted().toArray(Utterance[]::new));

			for (final Entry<String, GameHistory> gameHistory : gameHistories.entrySet()) {
				final String gameId = gameHistory.getKey();
				LOGGER.debug("Processing game \"{}\".", gameId);
				final GameHistory history = gameHistory.getValue();
				for (final String perspectivePlayerId : playerSourceIds.keySet()) {
					LOGGER.info("Processing game from perspective of player \"{}\".", perspectivePlayerId);

					utts.forEach(utt -> {
						final String uttPlayerId = uttPlayerIds.get(utt);
						if (perspectivePlayerId.equals(uttPlayerId)) {
							final Stream<GameContext> uttContexts = TemporalGameContexts.create(history,
									utt.getStartTime(), utt.getEndTime(), perspectivePlayerId);
							uttContexts.forEach(uttContext -> {
								SELECTED_ENTITY_ID_GETTER.apply(uttContext).ifPresent(selectedEntityId -> {
									// Add positive training examples
									addTokenInstances(utt, uttContext, positiveCtxFeatureExtractor,
											Boolean.TRUE.toString());
									// Add negative training examples
									addTokenInstances(utt, uttContext, negativeCtxFeatureExtractor,
											Boolean.FALSE.toString());
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

		private void addTokenInstances(final Utterance utt, final GameContext uttContext,
				final GameContextFeatureExtractor extractor, final String classValue) {
			utt.getTokens().forEach(token -> {
				final Instances classInstances = classInstanceGetter.apply(token);
				final Instance inst =  new DenseInstance(numAttributes);
				inst.setDataset(classInstances);
				extractor.accept(uttContext, inst);
				inst.setClassValue(classValue);
				classInstances.add(inst);
			});
		}
	}

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
						.hasArg().argName("path").type(File.class).required().build();
			}
		},
		RANDOM_SEED("r") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("random-seed")
						.desc("The value to use for seeding random negative examples.").hasArg().argName("value")
						.type(Number.class).build();
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
			formatter.printHelp(WordsAsClassifiersTrainingDataWriter.class.getSimpleName() + " INFILE", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final ArrayList<Attribute> ATTRS;

	private static final Attribute CLASS_ATTR;

	private static final String DEFAULT_OUTFILE_EXT = ".arff";

	private static final EntityFeature.Extractor EXTRACTOR;

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersTrainingDataWriter.class);

	static {
		final List<String> shapeFeatureVals = new ArrayList<>(IconImages.getImageResources().keySet());
		shapeFeatureVals.sort(Comparator.naturalOrder());
		EXTRACTOR = new EntityFeature.Extractor(shapeFeatureVals);
		final Map<EntityFeature, Attribute> featureAttrs = EXTRACTOR.getFeatureAttrs();
		ATTRS = new ArrayList<>(featureAttrs.size() + 1);
		ATTRS.addAll(featureAttrs.values());
		CLASS_ATTR = new Attribute("REFERENT", Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
		ATTRS.add(CLASS_ATTR);
	}

	public static void main(final CommandLine cl) throws IOException, JAXBException, ParseException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final File outpath = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
				LOGGER.info("Will write data to \"{}\".", outpath);
				final Number randomSeed = (Number) cl.getParsedOptionValue(Parameter.RANDOM_SEED.optName);
				final Random rnd;
				if (randomSeed == null) {
					LOGGER.info("Using default system-generated random seed.");
					rnd = new Random();
				} else {
					final long seed = randomSeed.longValue();
					LOGGER.info("Using {} as random seed.", seed);
					rnd = new Random(seed);
				}

				final WordsAsClassifiersTrainingDataWriter writer = new WordsAsClassifiersTrainingDataWriter(
						new RandomNotSelectedEntityIdGetter(rnd));
				final Map<String, Instances> classInstances = writer.apply(inpaths);
				if (outpath.mkdirs()) {
					LOGGER.info("Output directory \"{}\" was nonexistent; Created it before writing data.", outpath);
				}

				final AbstractFileSaver saver = ConverterUtils.getSaverForExtension(DEFAULT_OUTFILE_EXT);
				for (final Entry<String, Instances> classInstanceEntry : classInstances.entrySet()) {
					final String className = classInstanceEntry.getKey();
					final File outfile = new File(outpath, "wordAsClassifiers-" + className + DEFAULT_OUTFILE_EXT);
					LOGGER.info("Writing data for classifier \"{}\" to \"{}\".", className, outfile);
					final Instances insts = classInstanceEntry.getValue();
					saver.setInstances(insts);
					saver.setFile(outfile);
					saver.writeBatch();
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

	private static int estimateVocabTokenCount(final String token,
			final Map<Path, SessionDataManager> infileSessionData) {
		return infileSessionData.size() * 10;
	}

	private static int estimateVocabTypeCount(final Map<Path, SessionDataManager> infileSessionData) {
		return Math.toIntExact(Math.round(Math.ceil(Math.log(infileSessionData.size() * 850))));
	}

	private static void putSessionData(final Map<Path, SessionDataManager> fileSessionData, final Path infilePath)
			throws IOException {
		LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
		final Properties props = new Properties();
		try (final InputStream propsInstream = Files.newInputStream(infilePath)) {
			props.load(propsInstream);
			final Path infileBaseDir = infilePath.getParent();
			final SessionDataManager sessionData = SessionDataManager.create(props, infileBaseDir);
			fileSessionData.put(infilePath, sessionData);
		}
	}

	private final Function<GameContext, Integer> negativeExampleEntityIdGetter;

	/**
	 * @param rnd
	 */
	public WordsAsClassifiersTrainingDataWriter(final Function<GameContext, Integer> negativeExampleEntityIdGetter) {
		this.negativeExampleEntityIdGetter = negativeExampleEntityIdGetter;
	}

	public Map<String, Instances> apply(final Iterable<Path> inpaths) throws JAXBException, IOException {
		final Map<Path, SessionDataManager> infileSessionData = new HashMap<>();
		for (final Path inpath : inpaths) {
			LOGGER.info("Looking for batch job data underneath \"{}\".", inpath);
			final Path[] infiles = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
					.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
			for (final Path infile : infiles) {
				putSessionData(infileSessionData, infile);
			}
		}

		final Map<String, Instances> result = Maps
				.newHashMapWithExpectedSize(estimateVocabTypeCount(infileSessionData));
		final Function<String, Instances> classInstanceFetcher = className -> result.computeIfAbsent(className, k -> {
			final Instances instances = new Instances("referent_for_token-" + k, ATTRS,
					estimateVocabTokenCount(k, infileSessionData));
			instances.setClass(CLASS_ATTR);
			return instances;
		});
		final MultiClassDataCollector coll = new MultiClassDataCollector(classInstanceFetcher, ATTRS.size(),
				negativeExampleEntityIdGetter);
		for (final Entry<Path, SessionDataManager> infileSessionDataEntry : infileSessionData.entrySet()) {
			coll.accept(infileSessionDataEntry.getValue());
		}
		return result;
	}

}

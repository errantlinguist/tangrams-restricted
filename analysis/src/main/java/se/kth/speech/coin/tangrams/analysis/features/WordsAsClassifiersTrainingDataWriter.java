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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
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

import com.google.common.collect.Table;

import iristk.util.HAT;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameContextModelFactory;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.PlayerDataManager;
import se.kth.speech.coin.tangrams.analysis.SegmentUtteranceFactory;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContextFactory;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.analysis.UtterancePlayerIdMapFactory;
import se.kth.speech.coin.tangrams.iristk.io.LoggedEvents;
import se.kth.speech.hat.xsd.Annotation;
import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.AbstractFileSaver;
import weka.core.converters.ArffSaver;

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

	private static final EntityFeature.Extractor EXTRACTOR;

	private static final Map<EntityFeature, Attribute> FEATURE_ATTRS;

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersTrainingDataWriter.class);

	private static final SegmentUtteranceFactory SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	static {
		FEATURE_ATTRS = EntityFeature.Extractor.createFeatureAttrMap();
		EXTRACTOR = new EntityFeature.Extractor(FEATURE_ATTRS);
		ATTRS = new ArrayList<>(FEATURE_ATTRS.size() + 1);
		ATTRS.addAll(FEATURE_ATTRS.values());
		CLASS_ATTR = new Attribute("WORD", true);
		ATTRS.add(CLASS_ATTR);
	}

	public static void main(final CommandLine cl) throws IOException, JAXBException, ParseException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final Path[] inpaths = cl.getArgList().stream().map(Paths::get).toArray(Path[]::new);
			if (inpaths.length < 1) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final ArffSaver saver = new ArffSaver();
				final File outpath = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
				if (outpath == null) {
					LOGGER.info("Will write data to standard output stream.");
				} else {
					LOGGER.info("Will write data to \"{}\".", outpath);
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

	private final AbstractFileSaver saver;

	public WordsAsClassifiersTrainingDataWriter(final AbstractFileSaver saver) {
		this.saver = saver;
	}

	public void accept(final Path inpath) throws JAXBException, IOException {
		final Path[] infilePaths = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);

		final Instances instances = new Instances("word_training", ATTRS, infilePaths.length * 5000);
		instances.setClass(CLASS_ATTR);
		saver.setInstances(instances);

		for (final Path infilePath : infilePaths) {
			LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
			final Properties props = new Properties();
			try (final InputStream propsInstream = Files.newInputStream(infilePath)) {
				props.load(propsInstream);
			}
			accept(props, infilePath.getParent(), instances);
		}

		saver.writeBatch();
	}

	private void accept(final Properties props, final Path infileBaseDir, final Instances instances)
			throws JAXBException, IOException {
		final Path hatInfilePath = infileBaseDir.resolve(props.getProperty("hat"));
		LOGGER.info("Reading annotations from \"{}\".", hatInfilePath);
		final Annotation uttAnnots = HAT.readAnnotation(hatInfilePath.toFile());

		final PlayerDataManager playerData = PlayerDataManager.parsePlayerProps(props, infileBaseDir);

		final Table<String, String, GameHistory> gamePlayerHistoryTable = LoggedEvents.createPlayerGameHistoryTable(
				playerData.getPlayerEventLogs().entrySet(), LoggedEvents.VALID_MODEL_MIN_REQUIRED_EVENT_MATCHER);
		final int uniqueModelDescriptionCount = gamePlayerHistoryTable.values().size();
		final List<GameContextFeatureExtractor> contextFeatureExtractors = Arrays
				.asList(new SelectedEntityFeatureExtractor(EXTRACTOR,
						new GameContextModelFactory(uniqueModelDescriptionCount), new ImageEdgeCounter()));

		final Map<Utterance, String> uttPlayerIds = new UtterancePlayerIdMapFactory(SEG_UTT_FACTORY::create,
				playerData.getPlayerSourceIds().inverse()::get).apply(uttAnnots.getSegments().getSegment());
		final List<Utterance> utts = Arrays.asList(uttPlayerIds.keySet().stream().sorted().toArray(Utterance[]::new));

		for (final Entry<String, Map<String, GameHistory>> gamePlayerHistories : gamePlayerHistoryTable.rowMap()
				.entrySet()) {
			final String gameId = gamePlayerHistories.getKey();
			LOGGER.debug("Processing game \"{}\".", gameId);
			final Map<String, GameHistory> playerHistories = gamePlayerHistories.getValue();
			final TemporalGameContextFactory uttContextFactory = new TemporalGameContextFactory(playerHistories::get);
			for (final Entry<String, GameHistory> playerGameHistory : playerHistories.entrySet()) {
				final String playerId = playerGameHistory.getKey();
				LOGGER.info("Processing game from perspective of player \"{}\".", playerId);

				utts.forEach(utt -> {
					final Stream<Instance> uttInstances = createInstances(utt, uttPlayerIds::get, uttContextFactory,
							contextFeatureExtractors, instances);
					uttInstances.forEachOrdered(uttInstance -> {
						final double[] ctxFeatures = uttInstance.toDoubleArray();
						utt.getTokens().forEach(token -> {
							final Instance tokenInst = uttInstance.copy(ctxFeatures);
							tokenInst.setDataset(instances);
							tokenInst.setClassValue(token);
							instances.add(tokenInst);
						});
					});
				});
			}
		}
	}

	private Stream<Instance> createInstances(final Utterance utt,
			final Function<? super Utterance, String> uttPlayerIdGetter,
			final TemporalGameContextFactory uttContextFactory,
			final List<GameContextFeatureExtractor> contextFeatureExtractors, final Instances instances) {
		final String playerId = uttPlayerIdGetter.apply(utt);
		final Stream<GameContext> uttContexts = uttContextFactory.apply(utt.getStartTime(), utt.getEndTime(), playerId);
		return uttContexts.map(uttContext -> {
			final Instance inst = new DenseInstance(instances.numAttributes());
			inst.setDataset(instances);
			contextFeatureExtractors.forEach(extractor -> extractor.accept(uttContext, inst));
			return inst;
		});
	}

}

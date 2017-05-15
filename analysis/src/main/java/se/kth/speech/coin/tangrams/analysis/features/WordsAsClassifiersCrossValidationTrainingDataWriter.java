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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.function.Supplier;

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

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.EventDialogueFactory;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.GameContext.EntityStatus;
import se.kth.speech.coin.tangrams.analysis.GameContextModelFactory;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.RandomNotSelectedEntityIdGetter;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.TemporalGameContexts;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription.Datum;
import se.kth.speech.io.FileNames;
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
public final class WordsAsClassifiersCrossValidationTrainingDataWriter {

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
		OUTPUT_TYPE("t") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("output-type")
						.desc("The filename extension matching the data type to output (e.g. \"arff\" or \"arff.gz\").")
						.hasArg().argName("ext").build();
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

		private static String parseOutputType(final CommandLine cl) {
			String outExt = cl.getOptionValue(Parameter.OUTPUT_TYPE.optName, "arff");
			if (!outExt.startsWith(".")) {
				outExt = "." + outExt;
			}
			return outExt;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(WordsAsClassifiersCrossValidationTrainingDataWriter.class.getSimpleName() + " INFILE",
					OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	public static final String TEST_FILE_NAME_BASE = "test";

	public static final String TRAINING_FILE_NAME_PREFIX = "train-";

	private static final EventDialogueFactory EVENT_DIAG_FACTORY = new EventDialogueFactory(
			new EventTypeMatcher(GameManagementEvent.NEXT_TURN_REQUEST));

	private static final Logger LOGGER = LoggerFactory
			.getLogger(WordsAsClassifiersCrossValidationTrainingDataWriter.class);
	
	private static final EntityFeatureExtractionContextFactory EXTRACTION_CONTEXT_FACTORY = new EntityFeatureExtractionContextFactory(
			new GameContextModelFactory(1), new ImageEdgeCounter());

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
				final String outfileExt = Parameter.parseOutputType(cl);
				LOGGER.info("Will write data in \"*{}\" format.", outfileExt);

				final WordsAsClassifiersInstancesMapFactory instancesFactory = new WordsAsClassifiersInstancesMapFactory(EXTRACTION_CONTEXT_FACTORY,
						new RandomNotSelectedEntityIdGetter(rnd));
				final WordsAsClassifiersCrossValidationTrainingDataWriter writer = new WordsAsClassifiersCrossValidationTrainingDataWriter(
						instancesFactory, outpath, outfileExt);
				writer.accept(inpaths);
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

	private final WordsAsClassifiersInstancesMapFactory instancesFactory;

	private final File outdir;

	private final String outfileExt;

	public WordsAsClassifiersCrossValidationTrainingDataWriter(
			final WordsAsClassifiersInstancesMapFactory instancesFactory, final File outdir, final String outfileExt) {
		this.instancesFactory = instancesFactory;
		this.outdir = outdir;
		this.outfileExt = outfileExt;
	}

	public void accept(final Iterable<Path> inpaths) throws IOException, JAXBException {
		final Map<Path, SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths);
		final Collection<SessionDataManager> allSessions = infileSessionData.values();
		final Map<Path, String> infilePathOutdirNames = FileNames
				.createMinimalPathLeafNameMap(infileSessionData.keySet(), fileName -> {
					final String base = FileNames.splitBase(fileName)[0];
					return FileNames.sanitize(base, "-");
				});
		for (final Entry<Path, SessionDataManager> testSessionDataEntry : infileSessionData.entrySet()) {
			final Path testSessionDataFilePath = testSessionDataEntry.getKey();
			LOGGER.info("Creating {}-fold cross-validation set for testing on session data from \"{}\".",
					allSessions.size(), testSessionDataFilePath);

			final String subsampleDirname = infilePathOutdirNames.get(testSessionDataFilePath);
			final File subsampleDir = new File(outdir, subsampleDirname);
			LOGGER.info("Will write validation data to \"{}\".", subsampleDir);
			if (subsampleDir.mkdirs()) {
				LOGGER.debug("Subsample directory \"{}\" was nonexistent; Created it before writing data.",
						subsampleDir);
			}

			final SessionDataManager testSessionData = testSessionDataEntry.getValue();

			final Map<String, Instances> classInstances;
			{
				final SessionDataManager[] trainingSessionData = allSessions.stream()
						.filter(sessionData -> !sessionData.equals(testSessionData)).toArray(SessionDataManager[]::new);
				final List<SessionEventDialogueManager> trainingSessionEvtDiagMgrs = new ArrayList<>(
						trainingSessionData.length);
				for (final SessionDataManager trainingSessionDatum : trainingSessionData) {
					final SessionEventDialogueManager sessionEventDiagMgr = new SessionEventDialogueManager(
							trainingSessionDatum, EVENT_DIAG_FACTORY);
					trainingSessionEvtDiagMgrs.add(sessionEventDiagMgr);
				}
				classInstances = instancesFactory.apply(trainingSessionEvtDiagMgrs);
			}
			LOGGER.info("Trained classifiers for {} class(es); Writing to disk.", classInstances.size());

			final AbstractFileSaver saver = ConverterUtils.getSaverForExtension(outfileExt);
			for (final Entry<String, Instances> classInstanceEntry : classInstances.entrySet()) {
				final String className = classInstanceEntry.getKey();
				final File outfile = new File(subsampleDir, TRAINING_FILE_NAME_PREFIX + className + outfileExt);
				LOGGER.debug("Writing training data for classifier \"{}\" to \"{}\".", className, outfile);
				final Instances insts = classInstanceEntry.getValue();
				saver.setInstances(insts);
				saver.setFile(outfile);
				saver.writeBatch();
			}
			LOGGER.info("Wrote training data for {} class(es).", classInstances.size());

			createTestData(testSessionData);
			// final File testOutfile = new File(subsampleDir,
			// TEST_FILE_NAME_BASE + outfileExt);
			// LOGGER.info("Writing test data to \"{}\".", testOutfile);
			// final Instances testInsts =
			// instancesFactory.apply(Collections.singleton(testSessionData)).values()
			// .iterator().next();
			// saver.setInstances(testInsts);
			// saver.setFile(testOutfile);
			// saver.writeBatch();
			// LOGGER.info("Wrote {} test data point(s).",
			// testInsts.numInstances());
		}

		LOGGER.info("Finished writing {} cross-validation dataset(s) to \"{}\".", infileSessionData.size(), outdir);
	}

	private void createTestData(final SessionDataManager testSessionData) throws JAXBException, IOException {
		SessionEventDialogueManager sessionEventDiagMgr = new SessionEventDialogueManager(testSessionData,
				EVENT_DIAG_FACTORY);
		List<EventDialogue> uttDiags = sessionEventDiagMgr.createUttDialogues();
		for (EventDialogue uttDiag : uttDiags) {
			uttDiag.getLastEvent().ifPresent(event -> {
				LOGGER.debug("Creating test data for event: {}", event);
				final String submittingPlayerId = event.getString(GameManagementEvent.Attribute.PLAYER_ID.toString());
				final List<Utterance> dialogueUtts = uttDiag.getUtts();
				dialogueUtts.forEach(dialogueUtt -> {
					final String uttPlayerId = dialogueUtt.getSpeakerId();
					if (submittingPlayerId.equals(uttPlayerId)) {
						final GameHistory history = sessionEventDiagMgr.getGameHistory();
						final GameContext uttCtx = createGameContext(dialogueUtt, history, submittingPlayerId);
						Map<EntityStatus, Map<Integer, Datum>> imgVizInfoData = uttCtx
								.createEntityStatusVisualizationInfoMap();
						for (Entry<EntityStatus, Map<Integer, Datum>> imgVizInfoDataEntry : imgVizInfoData.entrySet()) {
							EntityStatus entityStatus = imgVizInfoDataEntry.getKey();
							Map<Integer, Datum> examples = imgVizInfoDataEntry.getValue();
							if (entityStatus == null) {
								for (Entry<Integer, Datum> negExample : examples.entrySet()) {

								}
							} else {
								switch (entityStatus) {
								case SELECTED: {
									for (Entry<Integer, Datum> posExample : examples.entrySet()) {
										
									}
									break;
								}
								default: {
									throw new IllegalArgumentException(
											String.format("No logic for enum value %s.", entityStatus));
								}
								}
							}
						}
					} else {
						LOGGER.debug(
								"Skipping the extraction of features for utterance with segment ID \"{}\" because the utterance is from player \"{}\" rather than the player whose perspective is being used for extracting features (\"{}\")",
								dialogueUtt.getSegmentId(), uttPlayerId, submittingPlayerId);
					}
				});
			});
		}
	}

	private static GameContext createGameContext(final Utterance dialogueUtt, final GameHistory history,
			final String perspectivePlayerId) {
		LOGGER.debug(
				"Creating a context based on the logged game history, which is then seen from the perspective of player \"{}\".",
				perspectivePlayerId);
		final GameContext[] ctxs = TemporalGameContexts
				.create(history, dialogueUtt.getStartTime(), dialogueUtt.getEndTime(), perspectivePlayerId)
				.toArray(GameContext[]::new);
		if (ctxs.length > 1) {
			LOGGER.warn("More than one game context found for {}; Only using the first one.", dialogueUtt);
		}
		return ctxs[0];
	}

}

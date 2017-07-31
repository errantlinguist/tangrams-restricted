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

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ExecutionException;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
import org.springframework.context.support.FileSystemXmlApplicationContext;

import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training.WordClassificationData;
import se.kth.speech.io.FileNames;
import weka.core.Instances;
import weka.core.converters.AbstractFileSaver;
import weka.core.converters.ConverterUtils;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see
 *      <ul>
 *      <li><a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.</li>
 *      <li><a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *      &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em><a>.</li>
 *      </ul>
 *
 */
final class TrainingDataWriter {

	private enum Parameter implements Supplier<Option> {
		APP_CONTEXT_DEFINITIONS("c") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("app-ctx")
						.desc("Location(s) to the Spring application context definition file(s) to load for configuration.")
						.hasArgs().argName("locator").required().build();
			}
		},
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
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(TrainingDataWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(TrainingDataWriter.class);

	private static final String TRAINING_FILE_NAME_PREFIX = "train-";

	public static void main(final String[] args) throws ExecutionException, IOException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static void main(final CommandLine cl) throws ParseException, ExecutionException, IOException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(String::trim).filter(path -> !path.isEmpty()).map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final File outpath = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
				LOGGER.info("Will write data to \"{}\".", outpath);
				final String outfileExt = CLIParameters
						.parseOutputType(cl.getOptionValue(Parameter.OUTPUT_TYPE.optName, "arff"));
				LOGGER.info("Will write data in \"*{}\" format.", outfileExt);
				final String[] appCtxLocs = CLIParameters
						.parseAppCtxDefPaths(cl.getOptionValues(Parameter.APP_CONTEXT_DEFINITIONS.optName)).stream()
						.toArray(String[]::new);
				try (final FileSystemXmlApplicationContext appCtx = new FileSystemXmlApplicationContext(appCtxLocs)) {
					final TestSetFactory testSetFactory = appCtx.getBean(TestSetFactory.class);
					final TrainingDataWriter writer = new TrainingDataWriter(testSetFactory, outpath, outfileExt);
					writer.accept(inpaths);
				}
			}
		}
	}

	private final File outdir;

	private final String outfileExt;

	private final TestSetFactory testSetFactory;

	private TrainingDataWriter(final TestSetFactory testSetFactory, final File outdir, final String outfileExt) {
		this.testSetFactory = testSetFactory;
		this.outdir = outdir;
		this.outfileExt = outfileExt;
	}

	private void accept(final Iterable<Path> inpaths) throws ExecutionException, IOException {
		final Map<Path, SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths);
		final Map<SessionDataManager, Path> allSessions = infileSessionData.entrySet().stream()
				.collect(Collectors.toMap(Entry::getValue, Entry::getKey));
		infileSessionData.forEach((infile, sessionData) -> allSessions.put(sessionData, infile));
		final Map<Path, String> infilePathOutdirNames = FileNames
				.createMinimalPathLeafNameMap(infileSessionData.keySet(), fileName -> {
					final String base = FileNames.splitBase(fileName)[0];
					return FileNames.sanitize(base, "-");
				});

		final Stream<Entry<SessionDataManager, WordClassificationData>> testSets = testSetFactory.apply(allSessions);
		for (final Iterator<Entry<SessionDataManager, WordClassificationData>> testSetIter = testSets
				.iterator(); testSetIter.hasNext();) {
			final Entry<SessionDataManager, WordClassificationData> testSet = testSetIter.next();
			final SessionDataManager testSessionData = testSet.getKey();
			final WordClassificationData trainingData = testSet.getValue();

			final Path sessionInpath = allSessions.get(testSessionData);
			final String subsampleDirname = infilePathOutdirNames.get(sessionInpath);
			final File subsampleDir = createSubsampleDir(subsampleDirname);
			final Map<String, Instances> classInsts = trainingData.getClassInstances();
			LOGGER.debug("Writing data for {} classes to \"{}\".", classInsts.size(), subsampleDir);
			persist(classInsts.entrySet(), subsampleDir);
		}
		LOGGER.info("Finished writing {} cross-validation dataset(s) to \"{}\".", infileSessionData.size(), outdir);
	}

	private File createSubsampleDir(final String subsampleDirname) {
		final File result = new File(outdir, subsampleDirname);
		LOGGER.info("Will write validation data to \"{}\".", result);
		if (result.mkdirs()) {
			LOGGER.debug("Subsample directory \"{}\" was nonexistent; Created it before writing data.", result);
		}
		return result;
	}

	private void persist(final Collection<Entry<String, Instances>> classInstances, final File subsampleDir)
			throws IOException {
		final AbstractFileSaver saver = ConverterUtils.getSaverForExtension(outfileExt);
		for (final Entry<String, Instances> classInstanceEntry : classInstances) {
			final String className = classInstanceEntry.getKey();
			final File outfile = new File(subsampleDir, TRAINING_FILE_NAME_PREFIX + className + outfileExt);
			LOGGER.debug("Writing training data for classifier \"{}\" to \"{}\".", className, outfile);
			final Instances insts = classInstanceEntry.getValue();
			saver.setInstances(insts);
			saver.setFile(outfile);
			saver.writeBatch();
		}
		LOGGER.info("Wrote training data for {} class(es).", classInstances.size());
	}
}

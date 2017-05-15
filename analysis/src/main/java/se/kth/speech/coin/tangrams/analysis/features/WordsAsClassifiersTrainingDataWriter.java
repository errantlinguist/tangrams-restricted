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

import se.kth.speech.coin.tangrams.analysis.EventDialogueFactory;
import se.kth.speech.coin.tangrams.analysis.RandomNotSelectedEntityIdGetter;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.iristk.EventTypeMatcher;
import se.kth.speech.coin.tangrams.iristk.GameManagementEvent;
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
				return Option.builder(optName).longOpt("outpath").desc("The path to write the data to.").hasArg()
						.argName("path").type(File.class).required().build();
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
			formatter.printHelp(WordsAsClassifiersTrainingDataWriter.class.getSimpleName() + " INFILE", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final EventDialogueFactory EVENT_DIAG_FACTORY = new EventDialogueFactory(
			new EventTypeMatcher(GameManagementEvent.NEXT_TURN_REQUEST));

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersTrainingDataWriter.class);

	public static void main(final CommandLine cl) throws IOException, JAXBException, ParseException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final File outdir = (File) cl.getParsedOptionValue(Parameter.OUTPATH.optName);
				LOGGER.info("Will write data to \"{}\".", outdir);
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

				final WordsAsClassifiersInstancesMapFactory instancesFactory = new WordsAsClassifiersInstancesMapFactory(
						new RandomNotSelectedEntityIdGetter(rnd));
				final WordsAsClassifiersTrainingDataWriter writer = new WordsAsClassifiersTrainingDataWriter(
						instancesFactory);
				final Map<String, Instances> classInstances = writer.apply(inpaths);
				if (outdir.mkdirs()) {
					LOGGER.info("Output directory \"{}\" was nonexistent; Created it before writing data.", outdir);
				}

				final AbstractFileSaver saver = ConverterUtils.getSaverForExtension(outfileExt);
				for (final Entry<String, Instances> classInstanceEntry : classInstances.entrySet()) {
					final String className = classInstanceEntry.getKey();
					final File outfile = new File(outdir, className + outfileExt);
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

	private final WordsAsClassifiersInstancesMapFactory instancesFactory;

	public WordsAsClassifiersTrainingDataWriter(final WordsAsClassifiersInstancesMapFactory instancesFactory) {
		this.instancesFactory = instancesFactory;
	}

	public Map<String, Instances> apply(final Iterable<Path> inpaths) throws IOException, JAXBException {
		final Collection<SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths)
				.values();
		final List<SessionEventDialogueManager> sessionEvtDiagMgrs = new ArrayList<>(infileSessionData.size());
		for (final SessionDataManager sessionDatum : infileSessionData) {
			final SessionEventDialogueManager sessionEventDiagMgr = new SessionEventDialogueManager(sessionDatum,
					EVENT_DIAG_FACTORY);
			sessionEvtDiagMgrs.add(sessionEventDiagMgr);
		}
		return instancesFactory.apply(sessionEvtDiagMgrs);

	}

}

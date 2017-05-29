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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
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
import org.springframework.context.support.FileSystemXmlApplicationContext;

import com.google.common.cache.LoadingCache;

import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManagerCacheSupplier;
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
public final class TrainingDataWriter {

	private enum Parameter implements Supplier<Option> {
		APP_CONTEXT_DEFINITIONS("a") {
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
			formatter.printHelp(TrainingDataWriter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(TrainingDataWriter.class);

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
				final String outfileExt = Parameter.parseOutputType(cl);
				LOGGER.info("Will write data in \"*{}\" format.", outfileExt);
				final String[] appCtxLocs = cl.getOptionValues(Parameter.APP_CONTEXT_DEFINITIONS.optName);
				try (final FileSystemXmlApplicationContext appCtx = new FileSystemXmlApplicationContext(appCtxLocs)) {
					final TrainingInstancesFactory instsFactory = appCtx.getBean(TrainingInstancesFactory.class);
					final SessionEventDialogueManagerCacheSupplier sessionDiagMgrCacheSupplier = appCtx
							.getBean(SessionEventDialogueManagerCacheSupplier.class);
					final TrainingDataWriter writer = new TrainingDataWriter(instsFactory, sessionDiagMgrCacheSupplier);
					final WordClassificationData trainingData = writer.apply(inpaths);
					if (outdir.mkdirs()) {
						LOGGER.info("Output directory \"{}\" was nonexistent; Created it before writing data.", outdir);
					}

					final AbstractFileSaver saver = ConverterUtils.getSaverForExtension(outfileExt);
					for (final Entry<String, Instances> classInstanceEntry : trainingData.getClassInstances()
							.entrySet()) {
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

	private final TrainingInstancesFactory instancesFactory;

	private final Supplier<LoadingCache<SessionDataManager, SessionEventDialogueManager>> sessionEvtDiagMgrSupplier;

	public TrainingDataWriter(final TrainingInstancesFactory instancesFactory,
			final Supplier<LoadingCache<SessionDataManager, SessionEventDialogueManager>> sessionEvtDiagMgrSupplier) {
		this.instancesFactory = instancesFactory;
		this.sessionEvtDiagMgrSupplier = sessionEvtDiagMgrSupplier;
	}

	public WordClassificationData apply(final Iterable<Path> inpaths) throws IOException, JAXBException {
		final Collection<SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths)
				.values();
		final List<SessionEventDialogueManager> sessionEvtDiagMgrs = new ArrayList<>(infileSessionData.size());
		for (final SessionDataManager sessionDatum : infileSessionData) {
			final SessionEventDialogueManager sessionEventDiagMgr = sessionEvtDiagMgrSupplier.get()
					.getUnchecked(sessionDatum);
			sessionEvtDiagMgrs.add(sessionEventDiagMgr);
		}
		return instancesFactory.apply(sessionEvtDiagMgrs);

	}

}

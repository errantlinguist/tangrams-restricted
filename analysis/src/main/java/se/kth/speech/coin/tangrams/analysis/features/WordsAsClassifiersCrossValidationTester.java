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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;

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
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import weka.classifiers.functions.Logistic;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see <a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *      &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *      Application of Perceptually Grounded Word Meanings for Incremental
 *      Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *      Annual Meeting of the Association for Computational Linguistics and the
 *      7<sup>th</sup> International Joint Conference on Natural Language
 *      Processing</em><a>.
 *
 */
public final class WordsAsClassifiersCrossValidationTester {

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
			formatter.printHelp(WordsAsClassifiersCrossValidationTester.class.getSimpleName() + " INFILE", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	public static final String TEST_FILE_NAME_BASE = "test";

	public static final String TRAINING_FILE_NAME_PREFIX = "train-";

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersCrossValidationTester.class);

	private static final String TEST_INSTS_REL_NAME = "tested_entites";

	public static void main(final CommandLine cl)
			throws ParseException, TrainingException, ExecutionException, IOException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				try (final ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext("extraction.xml",
						WordsAsClassifiersCrossValidationTester.class)) {
					final WordsAsClassifiersCrossValidationTester bean = appCtx
							.getBean(WordsAsClassifiersCrossValidationTester.class);
					bean.accept(inpaths);
				}
			}
		}
	}

	public static void main(final String[] args) throws TrainingException, ExecutionException, IOException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static Logistic createWordClassifier(final Instances data) throws TrainingException {
		final Logistic result = new Logistic();
		try {
			result.buildClassifier(data);
		} catch (final Exception e) {
			throw new TrainingException(e);
		}
		return result;
	}

	private static Map<String, Logistic> createWordClassifierMap(final Set<Entry<String, Instances>> classInstances)
			throws TrainingException {
		final Map<String, Logistic> result = Maps.newHashMapWithExpectedSize(classInstances.size());
		for (final Entry<String, Instances> classInstancesEntry : classInstances) {
			final String className = classInstancesEntry.getKey();
			LOGGER.debug("Training classifier for class \"{}\".", className);
			final Instances trainingInsts = classInstancesEntry.getValue();
			final Logistic classifier = createWordClassifier(trainingInsts);
			final Logistic oldClassifier = result.put(className, classifier);
			if (oldClassifier != null) {
				throw new IllegalArgumentException(
						String.format("More than one file for word class \"%s\".", className));
			}
			LOGGER.debug("{} instance(s) for class \"{}\".", trainingInsts.size(), className);
		}
		return result;
	}

	private static long estimateTestInstanceCount(final SessionDataManager sessionData) throws IOException {
		final long lineCount = Files.lines(sessionData.getCanonicalEventLogPath()).count();
		// Estimated number of entities per game * estimate number of tokens
		// (i.e. n-grams) per utterance dialogue
		return lineCount * 20 * 20;
	}

	@Inject
	private WordClassDiscountingSmoother smoother;

	@Inject
	private EntityCrossValidationTester tester;

	@Inject
	private ClassInstanceFactory testInstsFactory;

	@Inject
	private WordsAsClassifiersCrossValidationTestSetFactory testSetFactory;

	public void accept(final Iterable<Path> inpaths) throws TrainingException, ExecutionException, IOException {
		final Map<Path, SessionDataManager> infileSessionData = SessionDataManager.createFileSessionDataMap(inpaths);
		final Map<SessionDataManager, Path> allSessions = infileSessionData.entrySet().stream()
				.collect(Collectors.toMap(Entry::getValue, Entry::getKey));
		infileSessionData.forEach((infile, sessionData) -> allSessions.put(sessionData, infile));

		final Stream<Entry<SessionDataManager, Map<String, Instances>>> testSets = testSetFactory.apply(allSessions);
		for (final Iterator<Entry<SessionDataManager, Map<String, Instances>>> testSetIter = testSets
				.iterator(); testSetIter.hasNext();) {
			final Entry<SessionDataManager, Map<String, Instances>> testSet = testSetIter.next();
			final SessionDataManager testSessionData = testSet.getKey();

			final Map<String, Instances> classInstances = testSet.getValue();
			final Instances oovInstances = smoother.redistributeMass(classInstances);
			LOGGER.info("{} instances for out-of-vocabulary class.", oovInstances.size());

			final Map<String, Logistic> wordClassifiers = createWordClassifierMap(classInstances.entrySet());
			tester.setWordClassifiers(wordClassifiers::get);

			int initialCapacity = Integer.MAX_VALUE;
			{
				final long estimatedInstCount = estimateTestInstanceCount(testSessionData);
				try {
					initialCapacity = Math.toIntExact(estimatedInstCount);
				} catch (final ArithmeticException e) {
					LOGGER.debug(String.format("Could not convert long value \"%d\" to an int; Returning max.",
							estimatedInstCount), e);
				}
			}
			final Instances testInsts = testInstsFactory.apply(TEST_INSTS_REL_NAME, initialCapacity);
			tester.setTestInsts(testInsts);
			tester.test(testSessionData);
		}
	}

}

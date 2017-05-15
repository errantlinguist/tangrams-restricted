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
import java.io.UncheckedIOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

import com.google.common.collect.Maps;

import weka.classifiers.AbstractClassifier;
import weka.classifiers.functions.Logistic;
import weka.core.Attribute;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.AbstractFileLoader;
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
public final class WordsAsClassifiersCrossValidationTester {

	// private static class DisjointMultiClassifier extends AbstractClassifier {
	//
	// /**
	// *
	// */
	// private static final long serialVersionUID = -3227148575501596586L;
	//
	// private final List<String> classValues;
	//
	// private final Map<String, Logistic> wordClassifiers;
	//
	// public DisjointMultiClassifier() {
	// this(new HashMap<>(), new ArrayList<>());
	// }
	//
	// public DisjointMultiClassifier(final int initialCapacity) {
	// this(Maps.newHashMapWithExpectedSize(initialCapacity), new
	// ArrayList<>(initialCapacity));
	// }
	//
	// public DisjointMultiClassifier(final Map<String, Logistic>
	// wordClassifiers, final List<String> classValues) {
	// this.wordClassifiers = wordClassifiers;
	// this.classValues = classValues;
	// }
	//
	// /*
	// * (non-Javadoc)
	// *
	// * @see weka.classifiers.Classifier#buildClassifier(weka.core.Instances)
	// */
	// @Override
	// public void buildClassifier(final Instances data) throws
	// TrainingException {
	// final String className = parseRelationClassName(data.relationName());
	// final Logistic classifier = new Logistic();
	// try {
	// classifier.buildClassifier(data);
	// } catch (final Exception e) {
	// throw new TrainingException(e);
	// }
	// final Logistic oldClassifier = wordClassifiers.put(className,
	// classifier);
	// if (oldClassifier != null) {
	// throw new IllegalArgumentException(
	// String.format("Multiple training files found for class \"%s\".",
	// className));
	// } else {
	// classValues.add(className);
	// }
	// }
	//
	// /*
	// * (non-Javadoc)
	// *
	// * @see
	// * weka.classifiers.AbstractClassifier#distributionForInstance(weka.core
	// * .Instance)
	// */
	// @Override
	// public double[] distributionForInstance(final Instance instance) throws
	// Exception {
	// final double[] result = new double[classValues.size()];
	//
	// for (final ListIterator<String> classValueIter =
	// classValues.listIterator(); classValueIter.hasNext();) {
	// final int resultIdx = classValueIter.nextIndex();
	// final String classValue = classValueIter.next();
	// final Logistic classifier = wordClassifiers.get(classValue);
	// final double[] dist = classifier.distributionForInstance(instance);
	// // LOGGER.info("Distribution for class \"{}\": {}", classValue,
	// // Arrays.toString(dist));
	// final List<Object> possibleValues =
	// Collections.list(instance.classAttribute().enumerateValues());
	// final int truthValIdx = possibleValues.indexOf(Boolean.TRUE.toString());
	// final double truthVal = dist[truthValIdx];
	// LOGGER.debug("Confidence for class \"{}\" is {}.", classValue, truthVal);
	// result[resultIdx] = truthVal;
	// }
	//
	// return result;
	// }
	//
	// }

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

	private static class TestDescription {

		private final Map<Path, String> testFiles;

		private final Map<Path, String> trainingFiles;

		private TestDescription(final Map<Path, String> trainingFiles, final Map<Path, String> testFiles) {
			this.trainingFiles = trainingFiles;
			this.testFiles = testFiles;
		}
	}

	private static final Pattern CLASS_RELATION_NAME_PATTERN = Pattern
			.compile(Pattern.quote(WordsAsClassifiersInstancesMapFactory.CLASS_RELATION_PREFIX) + "(.+)");

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersCrossValidationTester.class);

	private static final Pattern TEST_FILE_NAME_PATTERN = Pattern.compile(
			Pattern.quote(WordsAsClassifiersCrossValidationTrainingDataWriter.TEST_FILE_NAME_BASE) + "(\\..+)");

	private static final Pattern TRAINING_FILE_NAME_PATTERN = Pattern
			.compile(Pattern.quote(WordsAsClassifiersCrossValidationTrainingDataWriter.TRAINING_FILE_NAME_PREFIX)
					+ "(.+?)(\\..+)");

	public static void main(final CommandLine cl)
			throws IOException, JAXBException, ParseException, TrainingException, ClassificationException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final List<Path> inpaths = Arrays.asList(cl.getArgList().stream().map(Paths::get).toArray(Path[]::new));
			if (inpaths.isEmpty()) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final WordsAsClassifiersCrossValidationTester writer = new WordsAsClassifiersCrossValidationTester();
				for (final Path inpath : inpaths) {
					writer.accept(inpath);
				}
			}
		}
	}

	// public static Map<String, Logistic> createWordClassifierMap(final
	// Map<Path, String> trainingFiles)
	// throws IOException, TrainingException {
	// final Map<String, Logistic> result =
	// Maps.newHashMapWithExpectedSize(trainingFiles.size());
	// for (final Entry<Path, String> trainingFileDesc :
	// trainingFiles.entrySet()) {
	// final Path infile = trainingFileDesc.getKey();
	// final String ext = trainingFileDesc.getValue();
	// LOGGER.info("Training model from \"{}\".", infile);
	// final AbstractFileLoader loader =
	// ConverterUtils.getLoaderForExtension(ext);
	// loader.setSource(infile.toFile());
	// final Instances structure = loader.getStructure();
	// final String className =
	// parseRelationClassName(structure.relationName());
	// for (Instance nextInst = loader.getNextInstance(structure); nextInst !=
	// null; nextInst = loader
	// .getNextInstance(structure)) {
	// structure.add(nextInst);
	// }
	// LOGGER.info("{} instance(s) for class \"{}\".", structure.numInstances(),
	// className);
	// final Attribute classAttr = structure
	// .attribute(WordsAsClassifiersCrossValidationTrainingDataWriter.CLASS_ATTR_NAME);
	// structure.setClassIndex(classAttr.index());
	// trainWordClassifier(result, className, structure);
	// }
	// return result;
	// }

	public static void main(final String[] args)
			throws IOException, JAXBException, TrainingException, ClassificationException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			main(cl);
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static TestDescription createTestDescription(final Path indir) throws IOException {
		LOGGER.info("Creating test description for directory \"{}\".", indir);
		final List<Path> dirFiles = Arrays.asList(Files.list(indir).filter(Files::isRegularFile).toArray(Path[]::new));
		final Map<Path, String> trainingFiles = new HashMap<>();
		final Map<Path, String> testFiles = new HashMap<>();
		for (final Path dirFile : dirFiles) {
			final String dirFileName = dirFile.getFileName().toString();
			final Matcher trainingFileMatcher = TRAINING_FILE_NAME_PATTERN.matcher(dirFileName);
			if (trainingFileMatcher.matches()) {
				final String ext = trainingFileMatcher.group(2);
				trainingFiles.put(dirFile, ext);
			} else {
				final Matcher testFileMatcher = TEST_FILE_NAME_PATTERN.matcher(dirFileName);
				if (testFileMatcher.matches()) {
					final String ext = testFileMatcher.group(1);
					testFiles.put(dirFile, ext);
				}
			}
		}
		assert !testFiles.isEmpty();
		assert !trainingFiles.isEmpty();
		return new TestDescription(trainingFiles, testFiles);
	}
	
	private static Logistic createWordClassifier(Instances data) throws TrainingException{
		final Logistic result = new Logistic();
		try {
			result.buildClassifier(data);
		} catch (final Exception e) {
			throw new TrainingException(e);
		}
		return result;
	}

	private static Map<String, Logistic> createWordClassifierMap(final Map<Path, String> trainingFiles)
			throws IOException, TrainingException {
		final Map<String, Logistic> result = Maps.newHashMapWithExpectedSize(trainingFiles.size());
		for (final Entry<Path, String> trainingFileDesc : trainingFiles.entrySet()) {
			final Path infile = trainingFileDesc.getKey();
			final String ext = trainingFileDesc.getValue();
			LOGGER.info("Training model from \"{}\".", infile);
			final AbstractFileLoader loader = ConverterUtils.getLoaderForExtension(ext);
			loader.setSource(infile.toFile());
			final Instances structure = loader.getStructure();
			final Attribute classAttr = structure.attribute(WordsAsClassifiersInstancesMapFactory.getClassAttrName());
			structure.setClassIndex(classAttr.index());
			for (Instance nextInst = loader.getNextInstance(structure); nextInst != null; nextInst = loader
					.getNextInstance(structure)) {
				structure.add(nextInst);
			}
			final String className = parseRelationClassName(structure.relationName());
			Logistic classifier = createWordClassifier(structure);
			Logistic oldClassifier = result.put(className, classifier);
			if (oldClassifier != null){
				throw new IllegalArgumentException(String.format("More than one file for word class \"%s\".", className));
			}
			LOGGER.info("{} instance(s) for class \"{}\".", structure.numInstances(), className);
		}
		return result;
	}

	private static boolean isTestDir(final Path dir) throws IOException {
		final List<Path> dirFiles = Arrays.asList(Files.list(dir).filter(Files::isRegularFile).toArray(Path[]::new));
		final boolean containsTestFiles = dirFiles.stream()
				.anyMatch(filePath -> TEST_FILE_NAME_PATTERN.matcher(filePath.getFileName().toString()).matches());
		return containsTestFiles && dirFiles.stream()
				.anyMatch(filePath -> TRAINING_FILE_NAME_PATTERN.matcher(filePath.getFileName().toString()).matches());
	}

	private static String parseRelationClassName(final String relName) {
		final Matcher classRelNameMatcher = CLASS_RELATION_NAME_PATTERN.matcher(relName);
		if (classRelNameMatcher.matches()) {
			return classRelNameMatcher.group(1);
		} else {
			throw new IllegalArgumentException(
					String.format("Could not parse a class name from relation name \"%s\".", relName));
		}
	}

	public void accept(final Path inpath) throws IOException, TrainingException, ClassificationException {
		LOGGER.info("Looking for training/testing data under \"{}\".", inpath);
		final Path[] indirs = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isDirectory)
				.filter(dirPath -> {
					try {
						return isTestDir(dirPath);
					} catch (final IOException e) {
						throw new UncheckedIOException(e);
					}
				}).toArray(Path[]::new);

		for (final Path indir : indirs) {
			final TestDescription testDesc = createTestDescription(indir);
			final Map<String,Logistic> wordClassifiers = createWordClassifierMap(testDesc.trainingFiles);
//			for (final Entry<Path, String> testFilePathExt : testDesc.testFiles.entrySet()) {
//				final AbstractFileLoader loader = ConverterUtils.getLoaderForExtension(testFilePathExt.getValue());
//				loader.setSource(testFilePathExt.getKey().toFile());
//				final Instances testStruct = loader.getStructure();
//				final Attribute classAttr = testStruct
//						.attribute(WordsAsClassifiersInstancesMapFactory.getClassAttrName());
//				testStruct.setClassIndex(classAttr.index());
//				for (Instance nextInst = loader.getNextInstance(testStruct); nextInst != null; nextInst = loader
//						.getNextInstance(testStruct)) {
//					try {
//						final double classification = wordClassifier.classifyInstance(nextInst);
//						LOGGER.info("Classification: " + classification);
//					} catch (final Exception e) {
//						throw new ClassificationException(e);
//					}
//					testStruct.add(nextInst);
//				}
//			}
//
//			final String testInfileStr = "C:\\Users\\tcshore\\Box Sync\\tangrams-restricted\\Ready\\20170329-1641-arvid-lutfisk\\events.lutfisk.txt";
		}
	}
}

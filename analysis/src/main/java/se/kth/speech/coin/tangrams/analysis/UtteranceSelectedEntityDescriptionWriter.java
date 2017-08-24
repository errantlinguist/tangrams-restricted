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
import java.io.UncheckedIOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;

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

import se.kth.speech.awt.LookAndFeels;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;
import se.kth.speech.coin.tangrams.analysis.dialogues.UtteranceDialogueRepresentationStringFactory;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;
import se.kth.speech.coin.tangrams.analysis.features.ImageEdgeCounter;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.view.UserPrompts;
import se.kth.speech.io.FileNames;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
final class UtteranceSelectedEntityDescriptionWriter { // NO_UCD (use default)

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
			return cl.getOptionValue(Parameter.OUTFILE_PREFIX.optName, DEFAULT_OUTFILE_PREFIX);
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

	private static final Path CLASS_SETTINGS_INFILE_PATH;

	private static final FileNameExtensionFilter DEFAULT_FILE_FILTER;

	private static final String DEFAULT_OUTFILE_PREFIX = "";

	private static final List<FileNameExtensionFilter> FILE_FILTERS;

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceSelectedEntityDescriptionWriter.class);

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
		final Path parent = inpath.getParent();
		final String defaultParentDirName = "ROOT_DIR";
		final String parentDirName;
		if (parent == null) {
			parentDirName = defaultParentDirName;
		} else {
			final Path parentFileName = parent.getFileName();
			if (parentFileName == null) {
				parentDirName = defaultParentDirName;
			} else {
				parentDirName = parentFileName.toString();
			}
		}

		final String fileBaseName;
		final Path fileName = inpath.getFileName();
		if (fileName == null) {
			fileBaseName = "";
		} else {
			fileBaseName = FileNames.splitBase(fileName.toString())[0];
		}
		return parentDirName + "-" + fileBaseName;
	}

	private static UtteranceSelectedEntityDescriptionWriter createWriter(final Path outpath,
			final String outfileNamePrefix, final boolean strict) {
		final Set<EntityFeature> featuresToDescribe = EnumSet.of(EntityFeature.EDGE_COUNT, EntityFeature.POSITION_X,
				EntityFeature.POSITION_Y);
		final List<EntityFeature> orderedFeaturesToDescribe = Arrays.asList(EntityFeature.getCanonicalOrdering().stream()
				.filter(featuresToDescribe::contains).toArray(EntityFeature[]::new));
		final UtteranceDialogueRepresentationStringFactory uttDiagReprFactory = new UtteranceDialogueRepresentationStringFactory(
				DataLanguageDefaults.getLocale());
		return new UtteranceSelectedEntityDescriptionWriter(new EntityFeature.Extractor(), orderedFeaturesToDescribe,
				uttDiagReprFactory, outpath, outfileNamePrefix, strict);
	}

	private static Settings loadClassSettings() {
		final Properties settingsProps = new Properties();
		try {
			loadClassSettingsProps(settingsProps);
		} catch (final IOException e) {
			LOGGER.debug(
					"A(n) {} occurred while trying to load the class settings from \"{}\"; Falling back to defaults.",
					e.getClass().getSimpleName(), CLASS_SETTINGS_INFILE_PATH);
		}
		return new Settings(settingsProps);
	}

	private static void loadClassSettingsProps(final Properties props) throws IOException {
		Files.createDirectories(SETTINGS_DIR);
		try (InputStream classSettingsPropsInstream = Files.newInputStream(CLASS_SETTINGS_INFILE_PATH)) {
			props.load(classSettingsPropsInstream);
		}
	}

	private static void main(final CommandLine cl) throws IOException, JAXBException, ParseException {
		if (cl.hasOption(Parameter.HELP.optName)) {
			Parameter.printHelp();
		} else {
			final Path[] inpaths = cl.getArgList().stream().map(String::trim).filter(path -> !path.isEmpty())
					.map(Paths::get).toArray(Path[]::new);
			if (inpaths.length < 1) {
				throw new MissingOptionException("No input path(s) specified.");

			} else {
				final Path outpath = ((File) cl.getParsedOptionValue(Parameter.OUTPATH.optName)).toPath();
				LOGGER.info("Will write data to \"{}\".", outpath);
				final boolean strict = cl.hasOption(Parameter.STRICT.optName);
				for (final Path inpath : inpaths) {
					LOGGER.info("Will read batch job data from \"{}\".", inpath);
					final String outfileNamePrefix = Parameter.parseOutfilePrefix(cl, inpath);
					LOGGER.info("Will prefix each output file for input \"{}\" with \"{}\".", inpath,
							outfileNamePrefix);
					final UtteranceSelectedEntityDescriptionWriter writer = createWriter(outpath, outfileNamePrefix,
							strict);
					writer.accept(inpath);
				}
			}

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
			LOGGER.info("Will read batch job data from \"{}\".", inpath);
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
							final UtteranceSelectedEntityDescriptionWriter writer = createWriter(outpath,
									outfileNamePrefix, false);
							try {
								writer.accept(inpath);
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

	private final EntityFeature.Extractor extractor;

	private final List<EntityFeature> featuresToDescribe;

	private final Path outdir;

	private final String outfileNamePrefix;

	private final boolean strict;

	private final Function<? super Iterator<Utterance>, String> uttDiagReprFactory;

	private UtteranceSelectedEntityDescriptionWriter(final EntityFeature.Extractor extractor,
			final List<EntityFeature> featuresToDescribe,
			final Function<? super Iterator<Utterance>, String> uttDiagReprFactory, final Path outdir,
			final String outfileNamePrefix, final boolean strict) {
		this.extractor = extractor;
		this.featuresToDescribe = featuresToDescribe;
		this.uttDiagReprFactory = uttDiagReprFactory;
		this.outdir = outdir;
		this.outfileNamePrefix = outfileNamePrefix;
		this.strict = strict;
	}

	public void accept(final Path inpath) throws JAXBException, IOException {
		final Path[] infilePaths = Files.walk(inpath, FileVisitOption.FOLLOW_LINKS).filter(Files::isRegularFile)
				.filter(filePath -> filePath.getFileName().toString().endsWith(".properties")).toArray(Path[]::new);
		for (final Path infilePath : infilePaths) {
			LOGGER.info("Reading batch job properties from \"{}\".", infilePath);
			final String outfileInfix = createOutfileInfix(infilePath);
			final SessionDataManager sessionData = SessionDataManager.create(infilePath);
			accept(sessionData, outfileNamePrefix + outfileInfix);
		}
	}

	private void accept(final SessionDataManager sessionData, final String outfileNamePrefix)
			throws JAXBException, IOException {
		final SessionGameManager sessionEvtDiagMgr = new SessionGameManager(sessionData);
		final EntityFeatureExtractionContextFactory extractionContextFactory = new EntityFeatureExtractionContextFactory(
				new GameContextModelFactory(2), new ImageEdgeCounter());
		final UtteranceTabularDataWriter gameWriter = new UtteranceTabularDataWriter(extractor, featuresToDescribe,
				extractionContextFactory, uttDiagReprFactory, strict);

		final Path extantOutdir = ensureExtantOutdir();
		for (final Entry<String, SessionGame> playerPerspectiveGame : sessionEvtDiagMgr.createPlayerPerspectiveGameMap()
				.entrySet()) {
			final SessionGame sessionGame = playerPerspectiveGame.getValue();
			final String playerId = playerPerspectiveGame.getKey();
			final String gameId = sessionGame.getGameId();
			final Path outfilePath = extantOutdir
					.resolve(outfileNamePrefix + "_GAME-" + gameId + "_LOG-" + playerId + ".tsv");
			LOGGER.info("Writing utterances from perspective of \"{}\" to \"{}\".", playerId, outfilePath);
			try (BufferedWriter writer = Files.newBufferedWriter(outfilePath, StandardOpenOption.CREATE,
					StandardOpenOption.TRUNCATE_EXISTING)) {
				gameWriter.accept(sessionGame, writer);
			}
		}

		final SessionGame canonicalSessionGame = sessionEvtDiagMgr.getCanonicalGame();
		final Path outfilePath = extantOutdir
				.resolve(outfileNamePrefix + "_GAME-" + canonicalSessionGame.getGameId() + "_CANONICAL.tsv");
		LOGGER.info("Writing utterances from canonical perspective to \"{}\".", outfilePath);
		try (BufferedWriter writer = Files.newBufferedWriter(outfilePath, StandardOpenOption.CREATE,
				StandardOpenOption.TRUNCATE_EXISTING)) {
			gameWriter.accept(canonicalSessionGame, writer);
		}
	}

	private Path ensureExtantOutdir() throws IOException {
		final boolean outdirAlreadyExists = Files.exists(outdir);
		final Path result = Files.createDirectories(outdir);
		if (!outdirAlreadyExists) {
			LOGGER.info("Created output directory \"{}\".", result);
		}
		return result;
	}
}

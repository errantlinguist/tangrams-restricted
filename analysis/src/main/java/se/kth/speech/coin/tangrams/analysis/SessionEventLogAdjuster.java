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

import java.awt.EventQueue;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.ForkJoinPool;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.awt.LookAndFeels;
import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.view.SessionEventLogAdjusterGUI;
import se.kth.speech.coin.tangrams.view.UserPrompts;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Aug 2017
 *
 */
final class SessionEventLogAdjuster {

	private static class Settings {

		private final Properties props;

		private Settings(final Properties props) {
			this.props = props;
		}

		public Optional<String> getInpath() {
			final String propVal = props.getProperty("inpath");
			return propVal == null ? Optional.empty() : Optional.of(propVal);
		}

		public void setInpath(final String propVal) {
			props.setProperty("inpath", propVal);
		}

		private Properties getProperties() {
			return props;
		}
	}

	private static final Path CLASS_SETTINGS_INFILE_PATH;

	private static final FileNameExtensionFilter DEFAULT_FILE_FILTER;

	private static final List<FileNameExtensionFilter> FILE_FILTERS;

	private static final Logger LOGGER = LoggerFactory.getLogger(SessionEventLogAdjuster.class);

	private static final Path SETTINGS_DIR;

	static {
		SETTINGS_DIR = Paths.get(".settings");
		CLASS_SETTINGS_INFILE_PATH = SETTINGS_DIR.resolve(SessionEventLogAdjuster.class.getName() + ".properties");
	}

	static {
		FILE_FILTERS = Arrays.asList(new FileNameExtensionFilter("Property files (*.properties)", "properties"));
		DEFAULT_FILE_FILTER = FILE_FILTERS.iterator().next();
	}

	public static void main(final String[] args) throws IOException, JAXBException {
		LookAndFeels.setLookAndFeel();
		if (args.length < 1) {
			final Optional<Path> inputInfile = promptInfile();
			if (inputInfile.isPresent()) {
				accept(inputInfile.get());
			}
		} else {
			final Path infile = Paths.get(args[0]);
			accept(infile);
		}
	}

	private static void accept(final Path infile) throws JAXBException, IOException {
		final SessionDataManager sessionData = SessionDataManager.create(infile);
		final SessionGameManager sessionEvtDiagMgr = new SessionGameManager(sessionData);
		final SessionGame canonicalGame = sessionEvtDiagMgr.getCanonicalGame();

		final Path infileParent = infile.getParent();
		final Optional<File> eventLogExportDir = infileParent == null ? Optional.empty()
				: Optional.of(infileParent.toFile());
		EventQueue.invokeLater(new SessionEventLogAdjusterGUI(canonicalGame, eventLogExportDir));
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

	private static Optional<Path> promptInfile() throws IOException {
		final Settings settings = loadClassSettings();
		final File currentInpath = new File(settings.getInpath().orElse(System.getProperty("user.dir")));
		final JFileChooser fileChooser = new JFileChooser(currentInpath);
		FILE_FILTERS.stream().forEachOrdered(fileChooser::addChoosableFileFilter);
		fileChooser.setFileFilter(DEFAULT_FILE_FILTER);
		fileChooser.setDialogTitle("Input file");
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		final Optional<Path> result = UserPrompts.promptFile(fileChooser).map(File::toPath);
		result.ifPresent(inpath -> {
			LOGGER.debug("Will read batch job data from \"{}\".", inpath);
			settings.setInpath(inpath.toString());

			ForkJoinPool.commonPool().submit(() -> {
				LOGGER.debug("Saving class settings to \"{}\".", CLASS_SETTINGS_INFILE_PATH);
				try (OutputStream settingsOutStream = Files.newOutputStream(CLASS_SETTINGS_INFILE_PATH,
						StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
					settings.getProperties().store(settingsOutStream, String
							.format("Persisted settings for class \"%s\".", SessionEventLogAdjuster.class.getName()));
				} catch (final IOException e) {
					LOGGER.error(String.format("A(n) %s occurred while persisting class settings to file.",
							e.getClass().getSimpleName()), e);
				}
			});
		});
		return result;
	}

	private SessionEventLogAdjuster() {
	}

}

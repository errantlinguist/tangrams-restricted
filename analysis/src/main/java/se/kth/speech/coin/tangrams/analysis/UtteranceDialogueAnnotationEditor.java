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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.util.ListIterator;
import java.util.Properties;
import java.util.function.BiFunction;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.view.UserPrompts;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 May 2017
 *
 */
public final class UtteranceDialogueAnnotationEditor {

	/**
	 * 
	 */
	public UtteranceDialogueAnnotationEditor() {
		// TODO Auto-generated constructor stub
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(UtteranceDialogueAnnotationEditor.class);
	
	public static void main(String[] args) {
		runInteractively();
	}
	
	@Inject
	private BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventDiagFactory;

	private static void runInteractively() {
		final JFileChooser fileChooser = new JFileChooser(System.getProperty("user.dir"));
		fileChooser.setDialogTitle("Input file");
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.setFileFilter(new FileNameExtensionFilter("Properties files (*.properties)", "properties"));
		UserPrompts.promptFile(fileChooser).map(File::toPath).ifPresent(inpath -> {
			LOGGER.info("Will read session data from \"{}\".", inpath);
			fileChooser.setDialogTitle("Output file");
			fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
			UserPrompts.promptFile(fileChooser).ifPresent(outpath -> {
				LOGGER.info("Will write data to \"{}\".", outpath);
				try {
					final SessionDataManager sessionData = SessionDataManager.create(inpath);
//					SessionEventDialogueManager evtDiagMgr = new SessionEventDialogueManager(sessionData);
				} catch (IOException e) {
					throw new UncheckedIOException(e);
				}
			});
		});
	}

}

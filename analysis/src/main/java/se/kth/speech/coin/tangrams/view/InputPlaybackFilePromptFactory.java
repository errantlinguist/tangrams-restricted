/*
 *  This file is part of se.kth.speech.coin.tangrams.playback.
 *
 *  se.kth.speech.coin.tangrams.playback is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.view;

import java.awt.Component;
import java.io.File;
import java.util.function.Function;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 30 Jan 2017
 *
 */
final class InputPlaybackFilePromptFactory implements Function<Component, File> {

	private static JFileChooser createFileChooser() {
		final JFileChooser result = new JFileChooser();
		final FileNameExtensionFilter filter = new FileNameExtensionFilter("Text files (*.txt)", "txt");
		result.setFileFilter(filter);
		return result;
	}

	private static JFileChooser createFileChooser(final File currentDir) {
		final JFileChooser result = createFileChooser();
		result.setCurrentDirectory(currentDir);
		return result;
	}

	private final JFileChooser chooser;

	private InputPlaybackFilePromptFactory(final JFileChooser chooser) {
		this.chooser = chooser;
	}

	InputPlaybackFilePromptFactory() {
		this(createFileChooser());
	}

	InputPlaybackFilePromptFactory(final File currentDir) {
		this(createFileChooser(currentDir));
	}

	@Override
	public File apply(final Component parent) {
		final File result;
		final int returnVal = chooser.showOpenDialog(parent);
		switch (returnVal) {
		case JFileChooser.APPROVE_OPTION: {
			result = chooser.getSelectedFile();
			break;
		}
		case JFileChooser.CANCEL_OPTION: {
			result = null;
			break;
		}
		case JFileChooser.ERROR_OPTION: {
			// TODO: Implement error handling
			result = null;
			break;
		}
		default: {
			throw new AssertionError(String.format("No logic for handling open-dialog value %d.", returnVal));
		}
		}

		return result;
	}

	/**
	 * @return the chooser
	 */
	public JFileChooser getChooser() {
		return chooser;
	}

}

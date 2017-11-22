/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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
import java.util.Optional;
import java.util.regex.Pattern;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 30 Apr 2017
 *
 */
public final class UserPrompts {

	private static final Pattern EMPTY_OR_WHITESPACE_PATTERN = Pattern.compile("\\s*");

	public static Optional<File> promptFile(final JFileChooser chooser) {
		return promptFile(chooser, null);
	}

	public static Optional<File> promptFile(final JFileChooser chooser, final Component parent) {
		Optional<File> result = Optional.empty();
		final int returnVal = chooser.showOpenDialog(parent);
		switch (returnVal) {
		case JFileChooser.APPROVE_OPTION: {
			result = Optional.of(chooser.getSelectedFile());
			break;
		}
		case JFileChooser.CANCEL_OPTION: {
			break;
		}
		case JFileChooser.ERROR_OPTION: {
			// TODO: Implement error handling
			break;
		}
		default: {
			throw new AssertionError(String.format("No logic for handling open-dialog value %d.", returnVal));
		}
		}
		return result;
	}

	public static Optional<String> promptNonBlankString(final String message, final String defaultIfBlank) {
		final Optional<String> result;
		final String input = JOptionPane.showInputDialog(message);
		// If the user cancels input, break out of the loop
		if (input == null) {
			result = Optional.empty();
		} else if (EMPTY_OR_WHITESPACE_PATTERN.matcher(input).matches()) {
			result = Optional.of(defaultIfBlank);
		} else {
			result = Optional.of(input);
		}
		return result;
	}

	private UserPrompts() {

	}

}

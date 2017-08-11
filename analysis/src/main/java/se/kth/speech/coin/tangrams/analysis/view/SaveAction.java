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
package se.kth.speech.coin.tangrams.analysis.view;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.Optional;

import javax.swing.AbstractAction;
import javax.swing.JFileChooser;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class SaveAction extends AbstractAction {

	private static final Logger LOGGER = LoggerFactory.getLogger(SaveAction.class);

	/**
	 *
	 */
	private static final long serialVersionUID = 3762393426054555238L;

	private final JFileChooser fileChooser;

	private final Frame parent;

	SaveAction(final JFileChooser fileChooser, final Frame parent) {
		this.parent = parent;
		this.fileChooser = fileChooser;
	}

	@Override
	public void actionPerformed(final ActionEvent e) {
		Optional<File> result = Optional.empty();
		final int returnVal = fileChooser.showSaveDialog(parent);
		switch (returnVal) {
		case JFileChooser.APPROVE_OPTION: {
			result = Optional.of(fileChooser.getSelectedFile());
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
		result.ifPresent(outputFile -> {
			LOGGER.info("Saving to \"{}\".", outputFile);
			// TODO: Finish
		});

	}
}
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

import java.io.IOException;
import java.io.StringWriter;
import java.io.UncheckedIOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableRowWriter;
import se.kth.speech.coin.tangrams.iristk.events.Move;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Aug 2017
 *
 */
final class ImageVisualizationInfoDescriptionFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(ImageVisualizationInfoDescriptionFactory.class);

	private final String blankDescription;

	private final Function<? super StringWriter, ImageVisualizationInfoTableRowWriter> imgVizInfoTableRowWriterFactory;

	ImageVisualizationInfoDescriptionFactory(
			final Function<? super StringWriter, ImageVisualizationInfoTableRowWriter> imgVizInfoTableRowWriterFactory) {
		this.imgVizInfoTableRowWriterFactory = imgVizInfoTableRowWriterFactory;
		blankDescription = createBlankDescription();
	}

	private String createBlankDescription() {
		final StringWriter strWriter = new StringWriter(16);
		final ImageVisualizationInfoTableRowWriter imgInfoDescWriter = imgVizInfoTableRowWriterFactory.apply(strWriter);
		try {
			imgInfoDescWriter.write(null, null);
			return strWriter.toString();
		} catch (final IOException e) {
			// Should not happen when writing to a StringWriter
			throw new UncheckedIOException(e);
		}
	}

	String createDescription(final Move move, final LocalDateTime gameStartTime,
			final List<ImageVisualizationInfo.Datum> imgVizInfoData) {
		final StringWriter strWriter = new StringWriter(256);
		final ImageVisualizationInfoTableRowWriter imgInfoDescWriter = imgVizInfoTableRowWriterFactory.apply(strWriter);
		final Integer selectedPieceId = move.getPieceId();
		final ImageVisualizationInfo.Datum selectedPieceImgVizInfo = imgVizInfoData.get(selectedPieceId);
		LOGGER.debug("Writing selected piece (ID {}) viz info: {} ", selectedPieceId, selectedPieceImgVizInfo);
		try {
			imgInfoDescWriter.write(selectedPieceId, selectedPieceImgVizInfo);
			return strWriter.toString();
		} catch (final IOException e) {
			// Should not happen when writing to a StringWriter
			throw new UncheckedIOException(e);
		}
	}

	/**
	 * @return the blankDescription
	 */
	String getBlankDescription() {
		return blankDescription;
	}

}

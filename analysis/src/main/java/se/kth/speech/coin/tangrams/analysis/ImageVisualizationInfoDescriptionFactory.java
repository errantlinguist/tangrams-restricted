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

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableRowCellFactory;
import se.kth.speech.coin.tangrams.iristk.events.Move;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Aug 2017
 *
 */
final class ImageVisualizationInfoDescriptionFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(ImageVisualizationInfoDescriptionFactory.class);

	private final String blankDescription;

	private final Collector<? super CharSequence, ?, String> rowCellJoiner;

	private final ImageVisualizationInfoTableRowCellFactory rowFactory;

	ImageVisualizationInfoDescriptionFactory(final ImageVisualizationInfoTableRowCellFactory rowFactory,
			final Collector<? super CharSequence, ?, String> rowCellJoiner) {
		this.rowFactory = rowFactory;
		this.rowCellJoiner = rowCellJoiner;
		blankDescription = createBlankDescription();
	}

	private String createBlankDescription() {
		final Stream<String> cellVals = rowFactory.createRowCellValues(null, null);
		return cellVals.collect(rowCellJoiner);
	}

	String createDescription(final Move move, final LocalDateTime gameStartTime,
			final List<ImageVisualizationInfo.Datum> imgVizInfoData) {
		final Integer selectedPieceId = move.getPieceId();
		final ImageVisualizationInfo.Datum selectedPieceImgVizInfo = imgVizInfoData.get(selectedPieceId);
		LOGGER.debug("Writing selected piece (ID {}) viz info: {} ", selectedPieceId, selectedPieceImgVizInfo);
		final Stream<String> cellVals = rowFactory.createRowCellValues(selectedPieceId, selectedPieceImgVizInfo);
		return cellVals.collect(rowCellJoiner);
	}

	/**
	 * @return the blankDescription
	 */
	String getBlankDescription() {
		return blankDescription;
	}

}

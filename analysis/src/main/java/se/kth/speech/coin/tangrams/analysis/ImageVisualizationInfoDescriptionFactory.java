/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfoTableRowCellFactory;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 23 Aug 2017
 *
 */
final class ImageVisualizationInfoDescriptionFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(ImageVisualizationInfoDescriptionFactory.class);

	private final List<String> blankDescription;

	private final ImageVisualizationInfoTableRowCellFactory rowFactory;

	ImageVisualizationInfoDescriptionFactory(final ImageVisualizationInfoTableRowCellFactory rowFactory) {
		this.rowFactory = rowFactory;
		blankDescription = Collections.unmodifiableList(Arrays.asList(createBlankDescription().toArray(String[]::new)));
	}

	private Stream<String> createBlankDescription() {
		return rowFactory.createRowCellValues(null, null);
	}

	Stream<String> createDescription(final int selectedPieceId, final LocalDateTime gameStartTime,
			final List<ImageVisualizationInfo.Datum> imgVizInfoData) {
		final ImageVisualizationInfo.Datum selectedPieceImgVizInfo = imgVizInfoData.get(selectedPieceId);
		LOGGER.debug("Writing selected piece (ID {}) viz info: {} ", selectedPieceId, selectedPieceImgVizInfo);
		return rowFactory.createRowCellValues(selectedPieceId, selectedPieceImgVizInfo);
	}

	/**
	 * @return the blankDescription
	 */
	List<String> getBlankDescription() {
		return blankDescription;
	}

}

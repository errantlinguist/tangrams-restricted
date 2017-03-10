/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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

import java.awt.Image;
import java.util.Collection;
import java.util.Map;
import java.util.Random;
import java.util.Map.Entry;
import java.util.function.BiFunction;

import com.google.common.collect.Maps;

import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanelFactory implements BiFunction<Collection<ImageVisualizationInfo>, Random, GameBoardPanel> {

	private final boolean allowFailedPlacements;

	private final int maxPlacementRetriesPerImg;

	GameBoardPanelFactory(final int maxPlacementRetriesPerImg, final boolean allowFailedPlacements) {
		this.maxPlacementRetriesPerImg = maxPlacementRetriesPerImg;
		this.allowFailedPlacements = allowFailedPlacements;
	}

	@Override
	public GameBoardPanel apply(final Collection<ImageVisualizationInfo> imgVisualizationInfoData, final Random rnd) {
		// TODO: make image count configurable
		final int maxImgPlacements = 20;
		final Map<Entry<? extends Image, ImageViewInfo>,Integer> pieceIds = Maps.newHashMapWithExpectedSize(imgVisualizationInfoData.size());
		final RandomImagePositionMatrixFiller matrixFiller = new RandomImagePositionMatrixFiller(rnd, maxImgPlacements,
				maxPlacementRetriesPerImg, allowFailedPlacements, (imgViewInfoDatum, pieceId) -> pieceIds.put(imgViewInfoDatum, pieceId));
		return new GameBoardPanel(imgVisualizationInfoData, matrixFiller, pieceIds::get);
	}

}

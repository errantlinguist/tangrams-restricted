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

import java.util.Map.Entry;
import java.util.Optional;

import se.kth.speech.MutablePair;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
public final class EntityStateDescriptions {

	public static Optional<Entry<ImageVisualizationInfoDescription.Datum, SpatialRegion>> createSelectedEntityDescription(
			final GameContext context, final SpatialMatrix<Integer> model) {
		final Optional<Integer> lastSelectedEntityId = context.findLastSelectedEntityId();
		return lastSelectedEntityId.map(entityId -> {
			final ImageVisualizationInfoDescription.Datum imgVizInfoDatum = context
					.getEntityVisualizationInfo(entityId);
			final SpatialRegion region = model.getElementPlacements().getElementMinimalRegions().get(entityId);
			return new MutablePair<>(imgVizInfoDatum, region);
		});
	}

	private EntityStateDescriptions() {

	}

}

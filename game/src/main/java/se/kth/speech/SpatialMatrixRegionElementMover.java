/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
 *
 *  se.kth.speech.coin.tangrams-restricted.client is free software: you can redistribute it and/or modify
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
package se.kth.speech;

import java.util.ArrayList;
import java.util.function.BiConsumer;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 11 Apr 2017
 *
 */
public final class SpatialMatrixRegionElementMover<E> implements BiConsumer<SpatialRegion, SpatialRegion> {

	private final SpatialMatrix<E> posMatrix;

	public SpatialMatrixRegionElementMover(final SpatialMatrix<E> posMatrix) {
		this.posMatrix = posMatrix;
	}

	@Override
	public void accept(final SpatialRegion source, final SpatialRegion target) {
		final SpatialMap<E> elemPlacements = posMatrix.getElementPlacements();
		final Iterable<E> elemsToMove = new ArrayList<>(elemPlacements.getMinimalRegionElements().get(source));
		for (final E elem : elemsToMove) {
			posMatrix.placeElement(elem, target);
		}
		posMatrix.clearRegion(source);
	}

}

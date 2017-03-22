/*
 *  This file is part of game.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams;

import java.util.function.Function;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.iristk.events.Area2D;
import se.kth.speech.coin.tangrams.iristk.events.CoordinatePoint2D;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Mar 18, 2017
 *
 */
public final class AreaSpatialRegionFactory implements Function<Area2D, SpatialRegion> {

	private final SpatialMatrix<?> model;

	public AreaSpatialRegionFactory(final SpatialMatrix<?> model) {
		this.model = model;
	}

	@Override
	public SpatialRegion apply(final Area2D area) {
		return createSpatialRegion(area.getStart(), area.getEnd());
	}

	private SpatialRegion createSpatialRegion(final CoordinatePoint2D start, final CoordinatePoint2D end) {
		return model.getRegion(start.getX(), end.getX(), start.getY(), end.getY());
	}

}

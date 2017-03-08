/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams.iristk;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import iristk.util.Record;
import se.kth.speech.coin.tangrams.game.Model;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Jan 2017
 *
 */
final class ModelDescription extends Record {

	private int colCount;

	/**
	 * This is a {@link List} of {@link String} instead of {@link Integer}
	 * instances in order to support (un-)marshalling of null objects.
	 */
	private List<String> coordOccupants;

	public ModelDescription() {
		// Default constructor is required for JSON (un-)marshalling
	}

	public ModelDescription(final Model<Integer> model) {
		setModel(model);
	}

	/**
	 * @return the colCount
	 */
	@RecordField(name = "colCount")
	public int getColCount() {
		return colCount;
	}

	/**
	 * @return the coordOccupants
	 */
	@RecordField(name = "coordOccupants")
	public List<String> getCoordOccupants() {
		return coordOccupants;
	}

	/**
	 * @param colCount
	 *            the colCount to set
	 */
	@RecordField(name = "colCount")
	public void setColCount(final int colCount) {
		this.colCount = colCount;
	}

	@RecordField(name = "coordOccupants")
	public void setCoordOccupants(final List<String> coordOccupants) {
		this.coordOccupants = coordOccupants;
	}

	/**
	 * @param model
	 */
	public void setModel(final Model<Integer> model) {
		final List<Integer> coordOccupants = model.getCoordinateOccupants().getValues();
		setCoordOccupants(coordOccupants.stream().map(Objects::toString).collect(Collectors.toList()));
		setColCount(model.getCoordinateDimensions()[1]);
	}

}

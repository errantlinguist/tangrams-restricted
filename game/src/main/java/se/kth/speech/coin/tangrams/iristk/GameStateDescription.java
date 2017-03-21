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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

import iristk.util.Record;
import se.kth.speech.coin.tangrams.game.PlayerRole;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Jan 2017
 *
 */
public final class GameStateDescription extends Record {

	private boolean allowFailedPlacements;

	private ModelDescription modelDescription;

	private double occupiedGridArea;

	@RecordField(name = "playerRoles")
	private List<List<String>> jsonPlayerRoles;

	/**
	 * This is a {@link String} instead of an {@link Long} reference in order to
	 * support (un-)marshalling.
	 */
	@RecordField(name = "seed")
	private String seed;

	public GameStateDescription() {
		// Default constructor is required for JSON (un-)marshalling
	}

	/**
	 * @return the allowFailedPlacements
	 */
	@RecordField(name = "allowFailedPlacements")
	public boolean allowFailedPlacements() {
		return allowFailedPlacements;
	}

	/**
	 * @return the modelDescription
	 */
	@RecordField(name = "modelDescription")
	public ModelDescription getModelDescription() {
		return modelDescription;
	}

	/**
	 * @return the occupiedGridArea
	 */
	@RecordField(name = "occupiedGridArea")
	public double getOccupiedGridArea() {
		return occupiedGridArea;
	}

	/**
	 * @return the playerRoles
	 */
	public BiMap<PlayerRole, String> getPlayerRoles() {
		final BiMap<PlayerRole, String> result = HashBiMap.create(jsonPlayerRoles.size());
		jsonPlayerRoles.stream().forEach(jsonPlayerRole -> {
			final Iterator<String> valIter = jsonPlayerRole.iterator();
			result.put(PlayerRole.valueOf(valIter.next()), valIter.next());
		});
		return result;
	}

	/**
	 * @return the seed
	 */
	public long getSeed() {
		return Long.parseLong(seed);
	}

	/**
	 * @param allowFailedPlacements
	 *            the allowFailedPlacements to set
	 */
	@RecordField(name = "allowFailedPlacements")
	public void setAllowFailedPlacements(final boolean allowFailedPlacements) {
		this.allowFailedPlacements = allowFailedPlacements;
	}

	/**
	 * @param modelDescription
	 *            the modelDescription to set
	 */
	@RecordField(name = "modelDescription")
	public void setModelDescription(final ModelDescription modelDescription) {
		this.modelDescription = modelDescription;
	}

	/**
	 * @param occupiedGridArea
	 *            the occupiedGridArea to set
	 */
	@RecordField(name = "occupiedGridArea")
	public void setOccupiedGridArea(final double occupiedGridArea) {
		this.occupiedGridArea = occupiedGridArea;
	}

	public void setPlayerRoles(final Map<PlayerRole, String> playerRoles) {
		final List<List<String>> jsonPlayerRoles = new ArrayList<>(playerRoles.size());
		playerRoles.forEach((role, playerId) -> {
			final List<String> pair = new ArrayList<>(2);
			pair.add(role.toString());
			pair.add(playerId);
			jsonPlayerRoles.add(pair);
		});
		this.jsonPlayerRoles = jsonPlayerRoles;
	}

	/**
	 * @param seed
	 */

	public void setSeed(final long seed) {
		this.seed = Long.toString(seed);

	}

}

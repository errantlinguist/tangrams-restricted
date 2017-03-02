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

import iristk.util.Record;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 10 Jan 2017
 *
 */
public final class GameStateDescription extends Record {

	private String activePlayerId;

	private ModelDescription modelDescription;

	private List<String> playerIds;

	/**
	 * This is a {@link String} instead of an {@link Long} reference in order to
	 * support (un-)marshalling.
	 */
	@RecordField(name = "seed")
	private String seed;

	/**
	 * This is a {@link List} of {@link String} instead of {@link Integer}
	 * instances in order to support (un-)marshalling of null objects.
	 */
	private List<String> winningConfiguration;

	public GameStateDescription() {
		// Default constructor is required for JSON (un-)marshalling
	}

	/**
	 * @return the activePlayerId
	 */
	@RecordField(name = "activePlayerId")
	public String getActivePlayerId() {
		return activePlayerId;
	}

	/**
	 * @return the modelDescription
	 */
	@RecordField(name = "modelDescription")
	public ModelDescription getModelDescription() {
		return modelDescription;
	}

	/**
	 * @return the playerIds
	 */
	@RecordField(name = "playerIds")
	public List<String> getPlayerIds() {
		return playerIds;
	}

	/**
	 * @return the seed
	 */
	public long getSeed() {
		return Long.parseLong(seed);
	}

	/**
	 * @return the winningConfiguration
	 */
	@RecordField(name = "winningConfiguration")
	public List<String> getWinningConfiguration() {
		return winningConfiguration;
	}

	/**
	 * @param activePlayerId
	 *            the activePlayerId to set
	 */
	@RecordField(name = "activePlayerId")
	public void setActivePlayerId(final String activePlayerId) {
		this.activePlayerId = activePlayerId;
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
	 * @param playerIds
	 *            the playerIds to set
	 */
	@RecordField(name = "playerIds")
	public void setPlayerIds(final List<String> playerIds) {
		this.playerIds = playerIds;
	}

	/**
	 * @param seed
	 */

	public void setSeed(final long seed) {
		this.seed = Long.toString(seed);

	}

	/**
	 * @param winningConfiguration
	 *            the winningConfiguration to set
	 */
	@RecordField(name = "winningConfiguration")
	public void setWinningConfiguration(final List<String> winningConfiguration) {
		this.winningConfiguration = winningConfiguration;
	}

}

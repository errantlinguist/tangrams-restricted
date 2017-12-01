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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.function.Function;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

import se.kth.speech.coin.tangrams.game.PlayerRole;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 12 Nov 2017
 *
 */
public final class UtteranceSpeakerParticipantIdMapFactory
		implements Function<Map<PlayerRole, String>, BiMap<String, String>> {

	public static final List<PlayerRole> DEFAULT_PLAYER_ROLE_ORDERING = Collections // NO_UCD (use private)
			.unmodifiableList(createDefaultPlayerRoleOrderingList());

	public static final List<String> DEFAULT_VALID_PARTICIPANT_IDS = Collections // NO_UCD (use private)
			.unmodifiableList(Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
					"P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"));

	private static List<PlayerRole> createDefaultPlayerRoleOrderingList() {
		final PlayerRole[] rolesToAdd = PlayerRole.values();
		final List<PlayerRole> result = new ArrayList<>(rolesToAdd.length);
		final PlayerRole initialRole = PlayerRole.MOVE_SUBMISSION;
		result.add(initialRole);
		for (final PlayerRole role : rolesToAdd) {
			if (!initialRole.equals(role)) {
				result.add(role);
			}
		}
		assert result.size() == rolesToAdd.length;
		return result;
	}

	private final List<PlayerRole> playerRoleOrdering;

	private final List<String> validParticipantIds;

	public UtteranceSpeakerParticipantIdMapFactory() {
		this(DEFAULT_PLAYER_ROLE_ORDERING, DEFAULT_VALID_PARTICIPANT_IDS);
	}

	public UtteranceSpeakerParticipantIdMapFactory(final List<PlayerRole> playerRoleOrdering, // NO_UCD (use private)
			final List<String> validParticipantIds) {
		this.playerRoleOrdering = playerRoleOrdering;
		this.validParticipantIds = validParticipantIds;
	}

	/**
	 * @param playerRoles
	 *            A {@link Map} mapping roles occupied by a particular player at
	 *            the beginning of the game to the respective player's username.
	 * @return A new {@link BiMap} mapping player usernames to anonymized
	 *         participant IDs representing the respective player.
	 */
	@Override
	public BiMap<String, String> apply(final Map<PlayerRole, String> playerRoles) {
		final Iterator<String> newParticipantIdIter = validParticipantIds.iterator();

		final BiMap<String, String> result = HashBiMap.create(Math.min(playerRoles.size(), playerRoleOrdering.size()));
		for (final PlayerRole role : playerRoleOrdering) {
			final String rolePlayerId = playerRoles.get(role);
			if (rolePlayerId != null) {
				try {
					final String nextParticipantId = newParticipantIdIter.next();
					result.put(rolePlayerId, nextParticipantId);
				} catch (final NoSuchElementException e) {
					final String msg = String.format(
							"There are more player roles to assign participant IDs for (%d) than possible participant IDs (%d).",
							playerRoles.size(), validParticipantIds.size());
					throw new IllegalArgumentException(msg, e);
				}

			}
		}
		return result;
	}

	/**
	 * @return the playerRoleOrdering
	 */
	public List<PlayerRole> getPlayerRoleOrdering() {
		return playerRoleOrdering;
	}

	/**
	 * @return the validParticipantIds
	 */
	public List<String> getValidParticipantIds() {
		return validParticipantIds;
	}

}

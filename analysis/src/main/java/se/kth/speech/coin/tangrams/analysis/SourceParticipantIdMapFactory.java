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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.BiFunction;

import org.apache.commons.lang3.tuple.Pair;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

import se.kth.speech.coin.tangrams.game.PlayerRole;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 13 Sep 2017
 *
 */
final class SourceParticipantIdMapFactory
		implements BiFunction<Map<String, String>, SessionGame, Entry<BiMap<String, String>, String>> {

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

	public SourceParticipantIdMapFactory() {
		this(DEFAULT_PLAYER_ROLE_ORDERING, DEFAULT_VALID_PARTICIPANT_IDS);
	}

	public SourceParticipantIdMapFactory(final List<PlayerRole> playerRoleOrdering, // NO_UCD (use private)
			final List<String> validParticipantIds) {
		this.playerRoleOrdering = playerRoleOrdering;
		this.validParticipantIds = validParticipantIds;
	}

	/**
	 * @param playerSourceIds
	 *            A {@link Map} mapping the username of each player in the given
	 *            game to the annotation audio source ID representing language
	 *            produced by that player.
	 * @param canonicalGame
	 *            The {@link SessionGame} representing the session to create
	 *            participant ID mappings for.
	 * @return A pair of a {@link BiMap} mapping audio source IDs to anonymized
	 *         participant IDs representing the respective player and a
	 *         {@link String} denoting the username of the player with the first
	 *         role according to the {@link #playerRoleOrdering supplied ordering}.
	 */
	@Override
	public Entry<BiMap<String, String>, String> apply(final Map<String, String> playerSourceIds,
			final SessionGame canonicalGame) {
		final BiMap<PlayerRole, String> playerRoles = canonicalGame.getHistory().getInitialState().getPlayerRoles();
		final BiMap<String, String> sourceParticipantIds = createSourceParticipantIdMap(playerSourceIds, playerRoles);
		return Pair.of(sourceParticipantIds,
				playerRoleOrdering.stream().map(playerRoles::get).filter(Objects::nonNull).findFirst().get());
	}

	private BiMap<String, String> createSourceParticipantIdMap(final Map<String, String> playerSourceIds,
			final Map<PlayerRole, String> playerRoles) {
		final Iterator<String> newParticipantIdIter = validParticipantIds.iterator();

		final BiMap<String, String> result = HashBiMap
				.create(Math.min(playerSourceIds.size(), Math.min(playerRoles.size(), playerRoleOrdering.size())));
		for (final PlayerRole role : playerRoleOrdering) {
			final String rolePlayerId = playerRoles.get(role);
			if (rolePlayerId != null) {
				final String sourceId = playerSourceIds.get(rolePlayerId);
				try {
					final String nextParticipantId = newParticipantIdIter.next();
					final String oldParticipantId = result.put(sourceId, nextParticipantId);
					assert oldParticipantId == null;
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

}

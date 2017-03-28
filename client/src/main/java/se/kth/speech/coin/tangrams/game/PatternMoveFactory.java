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
package se.kth.speech.coin.tangrams.game;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.RandomCollections;
import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.iristk.events.Move;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Mar 2017
 *
 */
public final class PatternMoveFactory
		implements Supplier<MapEntryRemapping<Integer, SpatialRegion>>, Controller.Listener {

	private static final Logger LOGGER = LoggerFactory.getLogger(PatternMoveFactory.class);

	private static final int PIECE_ID_LAST_SEEN_TURN_COUNT_OFFSET = 1;

	private final List<Integer> history;

	private final int initialRndOnsetLength;

	private final SpatialMatrix<Integer> posMatrix;

	private final Random rnd;

	private boolean shouldPickNewPiece = false;

	public PatternMoveFactory(final Random rnd, final SpatialMatrix<Integer> posMatrix,
			final int initialRndOnsetLength) {
		final int minInitialRndOnsetLength = PIECE_ID_LAST_SEEN_TURN_COUNT_OFFSET + 1;
		if (initialRndOnsetLength < minInitialRndOnsetLength) {
			throw new IllegalArgumentException(
					String.format("Initial random onset length was %d but must be at least %d.", initialRndOnsetLength,
							minInitialRndOnsetLength));
		}
		this.rnd = rnd;
		this.posMatrix = posMatrix;
		this.initialRndOnsetLength = initialRndOnsetLength;
		history = new LinkedList<>();
	}
	
	private boolean isStillBuildingInitialOnset(){
		return history.size() < initialRndOnsetLength;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public MapEntryRemapping<Integer, SpatialRegion> get() throws NoSuchElementException {
		MapEntryRemapping<Integer, SpatialRegion> result;
		if (isStillBuildingInitialOnset()) {
			LOGGER.debug("Still creating initial random onset: {}", history);
			// The first n pieces should be picked randomly
			final Collection<Integer> pieceIds = posMatrix.getElementPlacements().getAllElements();
			result = createRandomMove(pieceIds, pieceId -> !history.contains(pieceId));
		} else {
			if (shouldPickNewPiece) {
				LOGGER.debug("Picking previously-unmoved piece.");
				final Collection<Integer> pieceIds = posMatrix.getElementPlacements().getAllElements();
				result = createRandomMove(pieceIds, pieceId -> !history.contains(pieceId));
			} else {
				LOGGER.debug("Picking already-seen piece from history: {}", history);
				// Pick a random element from the turns before the last one
				final Collection<Integer> pieceIds = history.subList(0,
						history.size() - PIECE_ID_LAST_SEEN_TURN_COUNT_OFFSET);
				result = createRandomMove(pieceIds, pieceId -> true);
				// "pop" the result from its current position in the history
				history.remove(result.getKey());
			}
			shouldPickNewPiece = !shouldPickNewPiece;
		}
		if (result == null) {
			throw new NoSuchElementException("Could not find any valid moves.");
		} else {
			// (Re-)add the result to the end of the history
			history.add(result.getKey());
			return result;
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("PatternMoveFactory [posMatrix=");
		builder.append(posMatrix);
		builder.append(", rnd=");
		builder.append(rnd);
		builder.append(", history=");
		builder.append(history);
		builder.append(", initialRndOnsetLength=");
		builder.append(initialRndOnsetLength);
		builder.append("]");
		return builder.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateNextMove(se.
	 * kth.speech.coin.tangrams.iristk.events.Move)
	 */
	@Override
	public void updateNextMove(final Move move) {
		final Integer pieceId = move.getPieceId();
		LOGGER.debug("Notified of piece ID \"{}\" being moved; (Re-)adding to history.", pieceId);
		if (isStillBuildingInitialOnset()){
			LOGGER.debug("Still building initial random onset: {}", history);
		} else {
			final boolean wasSeenBefore = history.remove(pieceId);
			assert wasSeenBefore != shouldPickNewPiece;
			shouldPickNewPiece = !shouldPickNewPiece;
		}
		history.add(pieceId);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updatePlayerJoined(
	 * java.lang.String, long)
	 */
	@Override
	public void updatePlayerJoined(final String joinedPlayerId, final long time) {
		LOGGER.debug("Ignoring event.");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updatePlayerRole(se.
	 * kth.speech.coin.tangrams.game.PlayerRole)
	 */
	@Override
	public void updatePlayerRole(final PlayerRole newRole) {
		LOGGER.debug("Ignoring event.");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see se.kth.speech.coin.tangrams.game.Controller.Listener#
	 * updatePlayerSelection(java.lang.Integer, se.kth.speech.SpatialRegion)
	 */
	@Override
	public void updatePlayerSelection(final Integer pieceId, final SpatialRegion region) {
		LOGGER.debug("Ignoring event.");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateScore(int)
	 */
	@Override
	public void updateScore(final int score) {
		LOGGER.debug("Ignoring event.");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see se.kth.speech.coin.tangrams.game.Controller.Listener#
	 * updateSelectionRejected(java.lang.Integer, se.kth.speech.SpatialRegion)
	 */
	@Override
	public void updateSelectionRejected(final Integer pieceId, final SpatialRegion region) {
		LOGGER.debug("Ignoring event.");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateTurnCompleted(
	 * se.kth.speech.coin.tangrams.game.Turn)
	 */
	@Override
	public void updateTurnCompleted(final Turn turn) {
		LOGGER.debug("Ignoring event.");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateTurnCount(int)
	 */
	@Override
	public void updateTurnCount(final int newCount) {
		LOGGER.debug("Ignoring event.");
	}

	private MapEntryRemapping<Integer, SpatialRegion> createRandomMove(final Collection<Integer> pieceIds,
			final Predicate<? super Integer> pieceIdFilter) {
		LOGGER.debug("Creating random move.");
		// TODO: estimate number of failed tries better
		final Set<Integer> failedIds = Sets.newHashSetWithExpectedSize(Math.min(pieceIds.size(), 16));
		MapEntryRemapping<Integer, SpatialRegion> result = null;
		do {
			final Integer pieceId = RandomCollections.getRandomElement(pieceIds, rnd);
			if (pieceIdFilter.test(pieceId)) {
				result = createRandomMove(pieceId);
				if (result != null) {
					break;
				}
			} else {
				LOGGER.debug("Piece ID \"{}\" failed filter test; Cannot move again (yet).");
				failedIds.add(pieceId);
			}

		} while (result == null && failedIds.size() < pieceIds.size());
		return result;
	}

	private MapEntryRemapping<Integer, SpatialRegion> createRandomMove(final Integer pieceId) {
		final MapEntryRemapping<Integer, SpatialRegion> result;
		final SpatialRegion sourceRegion = posMatrix.getElementPlacements().getElementMinimalRegions().get(pieceId);
		final Set<SpatialRegion> possibleTargetRegions = posMatrix.createValidMoveSet(sourceRegion);
		if (possibleTargetRegions.isEmpty()) {
			LOGGER.debug("No valid moves for piece \"{}\", at {}.", pieceId, sourceRegion);
			result = null;
		} else {
			final SpatialRegion targetRegion = RandomCollections.getRandomElement(possibleTargetRegions, rnd);
			result = new MapEntryRemapping<>(pieceId, sourceRegion, targetRegion);
		}
		return result;
	}

}

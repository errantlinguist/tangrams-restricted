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

import java.util.Optional;
import java.util.Random;
import java.util.function.Function;

public final class RandomNotSelectedEntityIdGetter implements Function<GameContext, Optional<Integer>> {

	private final Random rnd;

	public RandomNotSelectedEntityIdGetter(final Random rnd) {
		this.rnd = rnd;
	}

	@Override
	public Optional<Integer> apply(final GameContext ctx) {
		final int notSelectedEntityId = applyAsInt(ctx);
		return Optional.of(notSelectedEntityId);
	}

	private int applyAsInt(final GameContext ctx) {
		final int entityCount = ctx.getEntityCount();
		final int result;
		final Optional<Integer> lastSelectedEntityId = ctx.findLastSelectedEntityId();
		if (lastSelectedEntityId.isPresent()) {
			final int selectedEntityId = lastSelectedEntityId.get();
			int entityId = rnd.nextInt(entityCount);
			while (selectedEntityId == entityId) {
				entityId = rnd.nextInt(entityCount);
			}
			result = entityId;
		} else {
			// Just pick any one
			result = rnd.nextInt(entityCount);
		}
		return result;
	}

}
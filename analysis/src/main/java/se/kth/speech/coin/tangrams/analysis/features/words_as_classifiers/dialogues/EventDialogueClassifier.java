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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.dialogues;

import java.util.Optional;

import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import se.kth.speech.coin.tangrams.analysis.GameContext;
import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.features.ClassificationException;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 May 2017
 *
 */
public interface EventDialogueClassifier {

	/**
	 * Calculates the confidence of an {@ink EventDialogue} referring to each
	 * referenceable entity given a particular {@link GameContext}.
	 *
	 * @param diag
	 *            The {@code EventDialogue} instance to classify.
	 * @param uttCtx
	 *            The {@link GameContext} instance representing the state of the
	 *            game at the time the given utterances were made.
	 * @return An {@link Optional} referring to a new {@link Int2DoubleMap}
	 *         mapping entity IDs to the confidence measure of the entity with
	 *         the given ID being referred to by the given utterances, or an
	 *         {@link Optional#empty() empty} one if no language in the given
	 *         dialogue was suitable for classification.
	 * @throws ClassificationException
	 *             If an error occurs while classifying any individual entity.
	 */
	Optional<Int2DoubleMap> apply(EventDialogue diag, final GameContext ctx) throws ClassificationException;

}

/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
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
package se.kth.speech.nlp.stanford;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import edu.stanford.nlp.ling.Label;
import edu.stanford.nlp.ling.Word;
import edu.stanford.nlp.trees.HeadFinder;
import edu.stanford.nlp.trees.Tree;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 28, 2017
 *
 */
public final class PhrasalHeadFilteringPredicate implements Predicate<Tree> {

	private final Map<? super String, ? extends Collection<? super List<String>>> labelHeadBlacklists;

	private final HeadFinder headFinder;

	public PhrasalHeadFilteringPredicate(
			final Map<? super String, ? extends Collection<? super List<String>>> labelHeadBlacklists,
			final HeadFinder headFinder) {
		this.labelHeadBlacklists = labelHeadBlacklists;
		this.headFinder = headFinder;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Predicate#test(java.lang.Object)
	 */
	@Override
	public boolean test(final Tree tree) {
		final Label label = tree.label();
		final String labelVal = label == null ? null : label.value();
		final Collection<? super List<String>> labelHeadBlacklist = labelHeadBlacklists.get(labelVal);

		final boolean result;
		if (labelHeadBlacklist == null) {
			result = true;
		} else {
			final Tree headTree = tree.headTerminal(headFinder);
			if (headTree == null) {
				result = true;
			} else {
				final List<String> tokens = Arrays
						.asList(headTree.yieldWords().stream().map(Word::word).toArray(String[]::new));
				result = !labelHeadBlacklist.contains(tokens);
			}
		}
		return result;
	}

}

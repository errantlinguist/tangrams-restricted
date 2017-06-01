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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.function.Function;

import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.CachingEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.ChainedEventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.EventDialogueTransformer;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.diags.InstructorUtteranceFilteringEventDialogueTransformer;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 1 Jun 2017
 *
 */
public final class UtteranceProcessorFactory implements Function<Executor, EventDialogueTransformer>, HasAbbreviation {

	private final Set<UtteranceProcessingOption> uttProcessingOptions;

	private static final List<UtteranceProcessingOption> PROCESSING_STEP_ORDERING = createProcessingStepOrdering();

	private static List<UtteranceProcessingOption> createProcessingStepOrdering() {
		List<UtteranceProcessingOption> result = Arrays.asList(UtteranceProcessingOption.INSTRUCTOR_ONLY,
				UtteranceProcessingOption.REMOVE_DISFLUENCIES, UtteranceProcessingOption.REMOVE_FILLERS,
				UtteranceProcessingOption.DEDUPLICATE_TOKENS, UtteranceProcessingOption.NPS_ONLY,
				UtteranceProcessingOption.PP_REMOVAL, UtteranceProcessingOption.LEMMATIZE,
				UtteranceProcessingOption.REMOVE_STOPWORDS);
		assert result.size() == UtteranceProcessingOption.values().length;
		return result;
	}

	/**
	 * 
	 */
	public UtteranceProcessorFactory(Set<UtteranceProcessingOption> uttProcessingOptions, Tokenization tokenization) {
		this.uttProcessingOptions = uttProcessingOptions;
	}

	private static final InstructorUtteranceFilteringEventDialogueTransformer INST_UTT_DIAG_TRANSFORMER = new InstructorUtteranceFilteringEventDialogueTransformer();

	private static final List<UtteranceProcessingOption> PARSING_OPTS = Arrays
			.asList(UtteranceProcessingOption.NPS_ONLY, UtteranceProcessingOption.PP_REMOVAL);

	private static final List<UtteranceProcessingOption> UNIFIABLE_PRE_PARSING_CLEANING_OPTS = Arrays.asList(
			UtteranceProcessingOption.REMOVE_DISFLUENCIES, UtteranceProcessingOption.REMOVE_FILLERS,
			UtteranceProcessingOption.DEDUPLICATE_TOKENS);

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public EventDialogueTransformer apply(Executor executor) {
		List<EventDialogueTransformer> chain = new ArrayList<>(uttProcessingOptions.size());
		if (uttProcessingOptions.contains(UtteranceProcessingOption.INSTRUCTOR_ONLY)) {
			chain.add(INST_UTT_DIAG_TRANSFORMER);
		}
		boolean isParsing = (PARSING_OPTS.stream().anyMatch(uttProcessingOptions::contains));
		if (isParsing) {
			// Add stopword filter to the chain after parsing in order to
			// prevent it from negatively affecting parsing accuracy
		} else {
			for (UtteranceProcessingOption preParsingOpt : UNIFIABLE_PRE_PARSING_CLEANING_OPTS) {
				if (uttProcessingOptions.contains(preParsingOpt)) {

				}
			}
		}
		// TODO Auto-generated method stub
		return new CachingEventDialogueTransformer(new ChainedEventDialogueTransformer(chain));
	}

	@Override
	public String getAbbreviation() {
		uttProcessingOptions.stream().map(UtteranceProcessingOption::getAbbreviation)
				.collect(TokenizationAbbreviations.JOINER);
		// TODO Auto-generated method stub
		return null;
	}

}

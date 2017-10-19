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
package se.kth.speech.nlp.google;

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

import com.google.cloud.language.v1.AnalyzeSyntaxRequest;
import com.google.cloud.language.v1.AnalyzeSyntaxResponse;
import com.google.cloud.language.v1.Document;
import com.google.cloud.language.v1.Document.Type;
import com.google.cloud.language.v1.EncodingType;
import com.google.cloud.language.v1.LanguageServiceClient;
import com.google.cloud.language.v1.PartOfSpeech;
import com.google.cloud.language.v1.TextSpan;
import com.google.cloud.language.v1.Token;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 18 Oct 2017
 *
 */
public final class DependencyExtractingTokenizer implements Function<String, List<String>> {

	private static final Set<PartOfSpeech.Tag> DEFAULT_BLACKLISTED_DEPENDENT_TAGS = EnumSet.of(PartOfSpeech.Tag.ADP,
			PartOfSpeech.Tag.VERB);

	private static final Set<PartOfSpeech.Tag> DEFAULT_HEAD_TAGS_TO_EXTRACT = EnumSet.of(PartOfSpeech.Tag.NOUN,
			PartOfSpeech.Tag.PRON);

	private static final EncodingType ENCODING_TYPE = EncodingType.UTF16;

	private static final String PARSING_LANGUAGE = "en";

	private static final Comparator<TextSpan> TEXT_SPAN_BEGIN_OFFSET_COMPARATOR = Comparator
			.comparingInt(TextSpan::getBeginOffset);

	private final LanguageServiceClient client;

	private final Predicate<? super Token> dependentTokenFilter;

	private final Predicate<? super Token> headTokenFilter;

	public DependencyExtractingTokenizer(final LanguageServiceClient client) {
		this(client, DEFAULT_HEAD_TAGS_TO_EXTRACT, DEFAULT_BLACKLISTED_DEPENDENT_TAGS);
	}

	public DependencyExtractingTokenizer(final LanguageServiceClient client,
			final Collection<? super PartOfSpeech.Tag> headTagsToExtract,
			final Collection<? super PartOfSpeech.Tag> blacklistedDependentTags) {
		this(client, token -> headTagsToExtract.contains(token.getPartOfSpeech().getTag()),
				tag -> !blacklistedDependentTags.contains(tag.getPartOfSpeech().getTag()));
	}

	public DependencyExtractingTokenizer(final LanguageServiceClient client,
			final Predicate<? super Token> headTokenFilter, final Predicate<? super Token> dependentTokenFilter) {
		this.client = client;
		this.headTokenFilter = headTokenFilter;
		this.dependentTokenFilter = dependentTokenFilter;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public List<String> apply(final String input) {
		final Document doc = Document.newBuilder().setContent(input).setType(Type.PLAIN_TEXT)
				.setLanguage(PARSING_LANGUAGE).build();
		final AnalyzeSyntaxRequest request = AnalyzeSyntaxRequest.newBuilder().setDocument(doc)
				.setEncodingType(ENCODING_TYPE).build();
		final AnalyzeSyntaxResponse response = client.analyzeSyntax(request);

		final List<Token> tokens = response.getTokensList();
		final TransitiveHeadSearcher headSearcher = new TransitiveHeadSearcher(tokens::get, headTokenFilter,
				Maps.newHashMapWithExpectedSize(tokens.size()), Math.min(tokens.size(), 8));
		final Set<Token> extractedTokens = Sets.newHashSetWithExpectedSize(Math.min(tokens.size(), 16));
		for (final Token token : tokens) {
			final TransitiveHeadSearcher.Result headSearchResult = headSearcher.apply(token);
			if (headSearchResult.wasHeadFound()) {
				final List<Token> chainToHead = headSearchResult.getChain();
				chainToHead.stream().filter(dependentTokenFilter).forEach(extractedTokens::add);
			}
		}
		return Arrays.asList(extractedTokens.stream().map(Token::getText).sorted(TEXT_SPAN_BEGIN_OFFSET_COMPARATOR)
				.map(TextSpan::getContent).toArray(String[]::new));
	}

}

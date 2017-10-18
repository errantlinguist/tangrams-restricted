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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.function.Predicate;

import com.google.cloud.language.v1.AnalyzeSyntaxRequest;
import com.google.cloud.language.v1.AnalyzeSyntaxResponse;
import com.google.cloud.language.v1.DependencyEdge;
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

	private static class TransitiveHeadSearchResults {

		private final ArrayList<Token> chain;

		private final boolean foundHead;

		private TransitiveHeadSearchResults(final ArrayList<Token> chain, final boolean foundHead) {
			this.chain = chain;
			this.foundHead = foundHead;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (!(obj instanceof TransitiveHeadSearchResults)) {
				return false;
			}
			final TransitiveHeadSearchResults other = (TransitiveHeadSearchResults) obj;
			if (chain == null) {
				if (other.chain != null) {
					return false;
				}
			} else if (!chain.equals(other.chain)) {
				return false;
			}
			if (foundHead != other.foundHead) {
				return false;
			}
			return true;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (chain == null ? 0 : chain.hashCode());
			result = prime * result + (foundHead ? 1231 : 1237);
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			final StringBuilder builder = new StringBuilder((chain.size() + 1) * 16);
			builder.append("TransitiveHeadSearchResults [chain=");
			builder.append(chain);
			builder.append(", foundHead=");
			builder.append(foundHead);
			builder.append("]");
			return builder.toString();
		}
	}

	private static final Set<PartOfSpeech.Tag> DEFAULT_BLACKLISTED_DEPENDENT_TAGS = EnumSet.of(PartOfSpeech.Tag.ADP,
			PartOfSpeech.Tag.VERB);

	private static final Set<PartOfSpeech.Tag> DEFAULT_HEAD_TAGS_TO_EXTRACT = EnumSet.of(PartOfSpeech.Tag.NOUN,
			PartOfSpeech.Tag.PRON);

	private static final EncodingType ENCODING_TYPE = EncodingType.UTF16;

	private static final String PARSING_LANGUAGE = "en";

	private static final Comparator<Token> TOKEN_BEGIN_OFFSET_COMPARATOR = Comparator
			.comparingInt(token -> token.getText().getBeginOffset());

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
		final Map<Token, Set<Token>> headDependentSets = Maps.newHashMapWithExpectedSize(Math.min(tokens.size(), 4));
		final int estimatedDependencyCountUpperBound = Math.max(tokens.size() - 1, 1);
		final Function<Token, Set<Token>> headDependentSetFactory = headToken -> Sets
				.newHashSetWithExpectedSize(Math.min(estimatedDependencyCountUpperBound, 4));

		// TODO: remove the token index counter once debugging messages are no
		// longer necessary
		int tokenIdx = 0;
		for (final Token token : tokens) {
			System.out.println(String.format("Token content \"%s\"; POS tag: \"%s\"; offset idx: \"%d\"",
					token.getText().getContent(), token.getPartOfSpeech().getTag(), tokenIdx));
			if (headTokenFilter.test(token)) {
				// The terminal token itself should be extracted
				headDependentSets.computeIfAbsent(token, headDependentSetFactory);
			} else if (token.hasDependencyEdge() && dependentTokenFilter.test(token)) {
				// Check if the head token the current token is dependent on
				// should be extracted, and thus by extension the current token
				// as well
				final DependencyEdge dependencyRel = token.getDependencyEdge();
				final int headTokenIdx = dependencyRel.getHeadTokenIndex();
				final Token headToken = response.getTokens(headTokenIdx);
				if (headTokenFilter.test(headToken)) {
					final Set<Token> dependents = headDependentSets.computeIfAbsent(headToken, headDependentSetFactory);
					System.out.println(String.format("Idx %d is dependent on idx %d.", tokenIdx, headTokenIdx));
					dependents.add(token);
				}
			}

			// System.out.println("Dependent chain: " +
			// dependencies.stream().map(Token::getText).map(TextSpan::getContent).collect(Collectors.joining("
			// ")));
			tokenIdx++;
		}

		final NavigableSet<Token> resultTokens = new TreeSet<>(TOKEN_BEGIN_OFFSET_COMPARATOR);
		for (final Entry<Token, Set<Token>> headDependents : headDependentSets.entrySet()) {
			final Token headToken = headDependents.getKey();
			final String headContent = headToken.getText().getContent();
			System.out.println(String.format("Dependents of \"%s\":", headContent));
			resultTokens.add(headToken);

			final Set<Token> dependents = headDependents.getValue();
			for (final Token dependent : dependents) {
				final String dependentContent = dependent.getText().getContent();
				System.out.println(String.format("Content: \"%s\"; POS tag: \"%s\"", dependentContent,
						dependent.getPartOfSpeech().getTag()));
				resultTokens.add(dependent);
			}
		}
		return Arrays
				.asList(resultTokens.stream().map(Token::getText).map(TextSpan::getContent).toArray(String[]::new));
	}

	private TransitiveHeadSearchResults findTransitiveHead(final Token token, final AnalyzeSyntaxResponse response) {
		Token currentToken = token;
		final ArrayList<Token> chain = new ArrayList<>();
		chain.add(currentToken);

		boolean foundHead = false;
		while (currentToken.hasDependencyEdge()) {
			final DependencyEdge dependencyEdge = currentToken.getDependencyEdge();
			final int headTokenIdx = dependencyEdge.getHeadTokenIndex();
			final Token headToken = response.getTokens(headTokenIdx);
			chain.add(headToken);
			if (foundHead = headTokenFilter.test(headToken)) {
				break;
			} else {
				currentToken = headToken;
			}
		}
		return new TransitiveHeadSearchResults(chain, foundHead);
	}

}

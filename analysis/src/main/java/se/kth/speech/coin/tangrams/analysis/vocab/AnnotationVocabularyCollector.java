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
package se.kth.speech.coin.tangrams.analysis.vocab;

import java.text.Collator;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.analysis.util.CharArraySet;

import se.kth.speech.coin.tangrams.analysis.SegmentUtteranceFactory;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.lucene.StringTokenizer;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 *
 */
public final class AnnotationVocabularyCollector
		implements Collector<Annotation, NavigableSet<String>, NavigableSet<String>> {

	private static final Set<Characteristics> CHARACTERISTICS = EnumSet.of(Characteristics.CONCURRENT,
			Characteristics.IDENTITY_FINISH, Characteristics.UNORDERED);

	private static final BinaryOperator<NavigableSet<String>> COMBINER = (c1, c2) -> {
		c1.addAll(c2);
		return c1;
	};

	private static StandardAnalyzer createAnalyzer(final Locale locale) {
		final CharArraySet stopwords = CharArraySet.EMPTY_SET;
		return new StandardAnalyzer(stopwords);
	}

	private static Function<String, List<String>> createDefaultTokenizer(final Locale locale) {
		final Analyzer analyzer = createAnalyzer(locale);
		final StringTokenizer wrapped = new StringTokenizer(analyzer);
		return content -> wrapped.apply(null, content).collect(Collectors.toList());
	}

	private final BiConsumer<NavigableSet<String>, Annotation> accumulator;

	private final Supplier<NavigableSet<String>> supplier;

	public AnnotationVocabularyCollector() {
		this(WordLists.DEFAULT_LOCALE);
	}

	public AnnotationVocabularyCollector(final Collator collator,
			final Function<? super String, ? extends List<String>> tokenizer) {
		// Use a TreeSet so that iteration order is stable across
		// invocations, meaning that a feature with a given index will
		// always have the same meaning
		supplier = () -> new TreeSet<>(collator);
		accumulator = new WordLists.WordAccumulator<>(new SegmentUtteranceFactory(tokenizer));
	}

	public AnnotationVocabularyCollector(final Locale locale) {
		this(Collator.getInstance(locale), createDefaultTokenizer(locale));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#accumulator()
	 */
	@Override
	public BiConsumer<NavigableSet<String>, Annotation> accumulator() {
		return accumulator;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#characteristics()
	 */
	@Override
	public Set<Characteristics> characteristics() {
		return CHARACTERISTICS;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#combiner()
	 */
	@Override
	public BinaryOperator<NavigableSet<String>> combiner() {
		return COMBINER;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#finisher()
	 */
	@Override
	public Function<NavigableSet<String>, NavigableSet<String>> finisher() {
		return Function.identity();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.stream.Collector#supplier()
	 */
	@Override
	public Supplier<NavigableSet<String>> supplier() {
		return supplier;
	}

}

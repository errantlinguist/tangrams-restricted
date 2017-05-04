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
package se.kth.speech.lucene;

import java.io.IOException;
import java.io.StringReader;
import java.io.UncheckedIOException;
import java.util.function.BiFunction;
import java.util.stream.Stream;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 *
 */
public final class StringTokenizer implements BiFunction<String, String, Stream<String>> {

	private final Analyzer analyzer;

	public StringTokenizer(final Analyzer analyzer) {
		this.analyzer = analyzer;
	}

	@Override
	public Stream<String> apply(final String field, final String content) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		try (TokenStream stream = analyzer.tokenStream(field, new StringReader(content))) {
			stream.reset();
			while (stream.incrementToken()) {
				resultBuilder.add(stream.getAttribute(CharTermAttribute.class).toString());
			}
		} catch (final IOException e) {
			throw new UncheckedIOException(e);
		}
		return resultBuilder.build();
	}

}

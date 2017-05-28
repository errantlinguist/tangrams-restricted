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
package se.kth.speech.nlp;

import java.util.List;
import java.util.function.Function;

import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.Annotator;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 14, 2017
 *
 */
public abstract class AbstractStanfordCoreNLPTokenizer implements Function<String, List<String>> {

	private final Annotator annotator;

	public AbstractStanfordCoreNLPTokenizer(final Annotator annotator) {
		this.annotator = annotator;

	}

	@Override
	public final List<String> apply(final String input) {
		final Annotation annot = annotate(input);
		return tokenize(annot);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("AbstractStanfordCoreNLPTokenizer [annotator=");
		builder.append(annotator);
		builder.append("]");
		return builder.toString();
	}

	private Annotation annotate(final String input) {
		final Annotation result = new Annotation(input);
		annotator.annotate(result);
		return result;
	}

	protected abstract List<String> tokenize(Annotation annot);

}

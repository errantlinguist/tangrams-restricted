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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training;

import java.util.Map;
import java.util.function.Function;

import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClasses;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Oct 2017
 *
 */
final class WordClassInstancesFetcher implements Function<String, Instances> {

	private final Map<String, Instances> classInstances;

	private final EntityInstanceAttributeContext entityInstAttrCtx;

	private final int estimatedVocabTypeTokenCount;

	WordClassInstancesFetcher(final Map<String, Instances> classInstances,
			final EntityInstanceAttributeContext entityInstAttrCtx, final int estimatedVocabTypeTokenCount) {
		this.classInstances = classInstances;
		this.entityInstAttrCtx = entityInstAttrCtx;
		this.estimatedVocabTypeTokenCount = estimatedVocabTypeTokenCount;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Instances apply(final String className) {
		return classInstances.computeIfAbsent(className, key -> {
			final Instances instances = new Instances(WordClasses.createRelationName(key), entityInstAttrCtx.getAttrs(),
					estimatedVocabTypeTokenCount);
			instances.setClass(entityInstAttrCtx.getClassAttr());
			return instances;
		});
	}

	/**
	 * @return the entityInstAttrCtx
	 */
	EntityInstanceAttributeContext getEntityInstAttrCtx() {
		return entityInstAttrCtx;
	}

	/**
	 * @return the estimatedVocabTypeTokenCount
	 */
	int getEstimatedVocabTypeTokenCount() {
		return estimatedVocabTypeTokenCount;
	}

}

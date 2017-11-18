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

import java.util.function.Function;

import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.WordClasses;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Oct 2017
 *
 */
final class WordClassInstancesFetcher implements Function<String, Instances> {

	private final Object2ObjectMap<String, WordClassificationData.Datum> classInstances;

	private final EntityInstanceAttributeContext entityInstAttrCtx;

	private final int estimatedVocabTypeTokenCount;

	WordClassInstancesFetcher(final Object2ObjectMap<String, WordClassificationData.Datum> classInstances,
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
		assert className != null;
		final WordClassificationData.Datum wordClassDatum = classInstances.computeIfAbsent(className, key -> {
			final Instances instances = new Instances(WordClasses.createRelationName(key), entityInstAttrCtx.getAttrs(),
					estimatedVocabTypeTokenCount);
			instances.setClass(entityInstAttrCtx.getClassAttr());
			return new WordClassificationData.Datum(instances);
		});
		return wordClassDatum.getTrainingInsts();
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
		if (!(obj instanceof WordClassInstancesFetcher)) {
			return false;
		}
		final WordClassInstancesFetcher other = (WordClassInstancesFetcher) obj;
		if (classInstances == null) {
			if (other.classInstances != null) {
				return false;
			}
		} else if (!classInstances.equals(other.classInstances)) {
			return false;
		}
		if (entityInstAttrCtx == null) {
			if (other.entityInstAttrCtx != null) {
				return false;
			}
		} else if (!entityInstAttrCtx.equals(other.entityInstAttrCtx)) {
			return false;
		}
		if (estimatedVocabTypeTokenCount != other.estimatedVocabTypeTokenCount) {
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
		result = prime * result + (classInstances == null ? 0 : classInstances.hashCode());
		result = prime * result + (entityInstAttrCtx == null ? 0 : entityInstAttrCtx.hashCode());
		result = prime * result + estimatedVocabTypeTokenCount;
		return result;
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
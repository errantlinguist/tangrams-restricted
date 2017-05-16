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
package se.kth.speech.coin.tangrams.analysis.features;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import weka.classifiers.Classifier;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 May 2017 * @see
 *        <a href="http://www.aclweb.org/anthology/P15-1029">Casey Kennington,
 *        &amp; David Schlangen. &ldquo;Simple Learning and Compositional
 *        Application of Perceptually Grounded Word Meanings for Incremental
 *        Reference Resolution&rdquo;. In <em>Proceedings of the 53<sup>rd</sup>
 *        Annual Meeting of the Association for Computational Linguistics and
 *        the 7<sup>th</sup> International Joint Conference on Natural Language
 *        Processing</em><a>.
 *
 */
public final class WordClassDiscountingSmoother {

	private static final Logger LOGGER = LoggerFactory.getLogger(WordClassDiscountingSmoother.class);

	@Inject
	private ClassInstanceFactory classInstsFactory;

	private final int minCount;

	private final String oovClassName;

	public WordClassDiscountingSmoother(final int minCount) {
		this(minCount, null);
	}

	public WordClassDiscountingSmoother(final int minCount, final String oovClassName) {
		this.oovClassName = oovClassName;
		this.minCount = minCount;
	}

	public List<Classifier> createNGramClassifierList(final List<String> wordClasses,
			final Function<? super String, ? extends Classifier> wordClassifiers) {
		return createNGramClassifierList(wordClasses, wordClassifiers, 1);
	}

	public Instances redistributeMass(final Map<String, Instances> classInsts) {
		return redistributeMass(classInsts, minCount, oovClassName);
	}

	private List<Classifier> createNGramClassifierList(final List<String> wordClasses,
			final Function<? super String, ? extends Classifier> wordClassifiers, final int ngramLength) {
		if (ngramLength != 1) {
			throw new IllegalArgumentException("Currently only unigram probabilities are supported.");
		}
		final List<Classifier> result = new ArrayList<>(wordClasses.size() * ngramLength);
		for (final String wordClass : wordClasses) {
			LOGGER.info("Getting classifier for class \"{}\".", wordClass);
			Classifier classifier = wordClassifiers.apply(wordClass);
			if (classifier == null) {
				LOGGER.debug("Getting distribution for OOV classes (\"{}\").", oovClassName);
				classifier = wordClassifiers.apply(oovClassName);
			}
			result.add(classifier);
		}
		return result;
	}

	private Instances redistributeMass(final Map<String, Instances> classInsts, final int minCount,
			final String augendClassName) {
		final List<Entry<String, Instances>> addendClassInsts = new ArrayList<>();
		final Iterator<Entry<String, Instances>> entryIter = classInsts.entrySet().iterator();
		while (entryIter.hasNext()) {
			final Entry<String, Instances> insts = entryIter.next();
			if (insts.getValue().numInstances() < minCount) {
				LOGGER.info("Class \"{}\" has fewer than {} instances; Will redistribute to \"{}\".",
						new Object[] { insts.getKey(), minCount, augendClassName });
				addendClassInsts.add(insts);
				entryIter.remove();
			}
		}
		final Instances augendInsts = classInsts.computeIfAbsent(augendClassName, k -> {
			final int totalInstCount = addendClassInsts.stream().map(Entry::getValue).mapToInt(Instances::numAttributes)
					.sum();
			return classInstsFactory.apply(WordsAsClassifiersInstancesMapFactory.createRelationName(augendClassName),
					totalInstCount);
		});
		addendClassInsts.stream().forEach(addendClassInst -> augendInsts.addAll(addendClassInst.getValue()));
		return augendInsts;
	}

}

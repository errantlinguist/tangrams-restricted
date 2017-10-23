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
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.training;

import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 26, 2017
 *
 */
public final class WordClassificationData {

	private final WordClassInstancesFetcher classInstancesFetcher;

	private final Map<String, Instances> classInsts;

	private final Object2IntMap<String> classObservationCounts;

	private final Object2IntMap<String> trainingInstanceCounts;

	public WordClassificationData(final WordClassificationData copyee) {
		classInsts = copyee.getClassInstances().entrySet().stream()
				.collect(Collectors.toMap(Entry::getKey, entry -> new Instances(entry.getValue())));

		final Object2IntMap<String> copyeeClassObservationCounts = copyee.getClassObservationCounts();
		classObservationCounts = new Object2IntOpenHashMap<>(copyeeClassObservationCounts);
		classObservationCounts.defaultReturnValue(copyeeClassObservationCounts.defaultReturnValue());

		final WordClassInstancesFetcher copyeeFetcher = copyee.getClassInstancesFetcher();
		classInstancesFetcher = new WordClassInstancesFetcher(classInsts, copyeeFetcher.getEntityInstAttrCtx(),
				copyeeFetcher.getEstimatedVocabTypeTokenCount());

		final Object2IntMap<String> copyeeTrainingInstanceCounts = copyee.getTrainingInstanceCounts();
		trainingInstanceCounts = new Object2IntOpenHashMap<>(copyeeTrainingInstanceCounts);
		trainingInstanceCounts.defaultReturnValue(copyeeTrainingInstanceCounts.defaultReturnValue());
	}

	WordClassificationData(final Map<String, Instances> classInsts, final Object2IntMap<String> classObservationCounts,
			final WordClassInstancesFetcher classInstancesFetcher) {
		this.classInsts = classInsts;
		this.classObservationCounts = classObservationCounts;
		this.classInstancesFetcher = classInstancesFetcher;

		trainingInstanceCounts = new Object2IntOpenHashMap<>(2);
		trainingInstanceCounts.defaultReturnValue(0);
	}

	/**
	 * @return the classInsts
	 */
	public Map<String, Instances> getClassInstances() {
		return classInsts;
	}

	/**
	 * @return the classObservationCounts
	 */
	public Object2IntMap<String> getClassObservationCounts() {
		return classObservationCounts;
	}

	/**
	 * @return the trainingInstanceCounts
	 */
	public Object2IntMap<String> getTrainingInstanceCounts() {
		return trainingInstanceCounts;
	}

	void addObservation(final String wordClass, final Stream<Entry<Instance, String>> instClassValues) {
		final Instances classInstances = classInstancesFetcher.apply(wordClass);
		instClassValues.forEach(instClassValue -> {
			final Instance inst = instClassValue.getKey();
			// NOTE: Caching individual Instance instances could only reduce
			// computation time and not memory footprint because
			// "Instances.add(..)" actually performs a shallow copy of each
			// Instance before adding it
			classInstances.add(inst);
			final String classValue = instClassValue.getValue();
			trainingInstanceCounts.put(classValue, trainingInstanceCounts.getInt(classValue) + 1);
		});
		classObservationCounts.put(wordClass, classObservationCounts.getInt(wordClass) + 1);
	}

	Instances fetchWordInstances(final String wordClass) {
		return classInstancesFetcher.apply(wordClass);
	}

	/**
	 * @return the classInstancesFetcher
	 */
	WordClassInstancesFetcher getClassInstancesFetcher() {
		return classInstancesFetcher;
	}

}

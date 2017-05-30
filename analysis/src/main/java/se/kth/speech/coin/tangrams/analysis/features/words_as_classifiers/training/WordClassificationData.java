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
import java.util.function.Function;
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

	private final Map<String, Instances> classInsts;

	private final Function<String, Instances> classInstancesFetcher;

	private final Object2IntMap<String> classObservationCounts;

	private final Object2IntMap<String> trainingInstanceCounts;

	protected WordClassificationData(final Map<String, Instances> classInsts,
			final Object2IntMap<String> classObservationCounts,
			final Function<String, Instances> classInstancesFetcher) {
		this.classInsts = classInsts;
		this.classObservationCounts = classObservationCounts;
		this.classInstancesFetcher = classInstancesFetcher;

		trainingInstanceCounts = new Object2IntOpenHashMap<>(2);
		trainingInstanceCounts.defaultReturnValue(0);
	}

	public Instances fetchWordInstances(final String wordClass) {
		return classInstancesFetcher.apply(wordClass);
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

	protected void addObservation(final String wordClass, final Stream<Entry<Instance, String>> instClassValues) {
		final Instances classInstances = classInstancesFetcher.apply(wordClass);
		instClassValues.forEach(instClassValue -> {
			final Instance inst = instClassValue.getKey();
			classInstances.add(inst);
			final String classValue = instClassValue.getValue();
			trainingInstanceCounts.put(classValue, trainingInstanceCounts.getInt(classValue) + 1);
		});
		classObservationCounts.put(wordClass, classObservationCounts.getInt(wordClass) + 1);
	}

}

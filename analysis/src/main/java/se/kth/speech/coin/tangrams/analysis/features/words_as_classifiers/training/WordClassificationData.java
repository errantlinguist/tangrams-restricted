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
import java.util.function.Function;
import java.util.stream.Stream;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
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

	protected WordClassificationData(final Map<String, Instances> classInsts,
			final Object2IntMap<String> classObservationCounts,
			final Function<String, Instances> classInstancesFetcher) {
		this.classInsts = classInsts;
		this.classObservationCounts = classObservationCounts;
		this.classInstancesFetcher = classInstancesFetcher;
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

	protected void addObservation(final String wordClass, final Stream<Instance> insts) {
		final Instances classInstances = classInstancesFetcher.apply(wordClass);
		insts.forEach(inst -> {
			classInstances.add(inst);
		});
		classObservationCounts.put(wordClass, classObservationCounts.getInt(wordClass) + 1);
	}

}

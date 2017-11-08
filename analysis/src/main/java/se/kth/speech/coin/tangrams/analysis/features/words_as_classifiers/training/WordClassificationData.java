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

	public static final class Datum {
		
		private int observationCount;
		
		private int trainingInstancesChangeCount;

		private final Instances trainingInsts;

		private Datum(final Datum copyee) {
			this(new Instances(copyee.getTrainingInsts()), copyee.getObservationCount(), copyee.getTrainingInstancesChangeCount());
		}

		public Datum(final Instances trainingInsts) {
			this(trainingInsts, 0, 0);
		}

		private Datum(final Instances trainingInsts, final int observationCount, int trainingInstancesChangeCount) {
			this.trainingInsts = trainingInsts;
			this.observationCount = observationCount;
			this.trainingInstancesChangeCount = trainingInstancesChangeCount;
		}

		public void add(final Datum other) {
			setObservationCount(getObservationCount() + other.getObservationCount());
			getTrainingInsts().addAll(other.getTrainingInsts());
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Datum other = (Datum) obj;
			if (observationCount != other.observationCount)
				return false;
			if (trainingInstancesChangeCount != other.trainingInstancesChangeCount)
				return false;
			if (trainingInsts == null) {
				if (other.trainingInsts != null)
					return false;
			} else if (!trainingInsts.equals(other.trainingInsts))
				return false;
			return true;
		}

		/**
		 * @return the observationCount
		 */
		public int getObservationCount() {
			return observationCount;
		}

		public int getTrainingInstancesChangeCount() {
			return trainingInstancesChangeCount;
		}

		/**
		 * @return the trainingInsts
		 */
		public Instances getTrainingInsts() {
			return trainingInsts;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + observationCount;
			result = prime * result + trainingInstancesChangeCount;
			result = prime * result + ((trainingInsts == null) ? 0 : trainingInsts.hashCode());
			return result;
		}

		private void incrementTrainingInstancesChangeCount() {
			this.trainingInstancesChangeCount += 1;
		}

		/**
		 * @param observationCount
		 *            the observationCount to set
		 */
		private void setObservationCount(final int observationCount) {
			this.observationCount = observationCount;
		}
	}

	private final Map<String, Datum> classData;

	private final WordClassInstancesFetcher classInstancesFetcher;

	/**
	 * The counts of training {@link Instance instances} for each classification
	 * type, i.e.&nbsp; {@code "true"} or {@code "false"}.
	 */
	private final Object2IntMap<String> trainingInstanceCounts;

	WordClassificationData(final Map<String, Datum> classData, final WordClassInstancesFetcher classInstancesFetcher) {
		this.classData = classData;
		this.classInstancesFetcher = classInstancesFetcher;

		trainingInstanceCounts = new Object2IntOpenHashMap<>(2);
		trainingInstanceCounts.defaultReturnValue(0);
	}

	WordClassificationData(final WordClassificationData copyee) {
		classData = copyee.getClassData().entrySet().stream()
				.collect(Collectors.toMap(Entry::getKey, entry -> new Datum(entry.getValue())));

		final WordClassInstancesFetcher copyeeFetcher = copyee.getClassInstancesFetcher();
		classInstancesFetcher = new WordClassInstancesFetcher(classData, copyeeFetcher.getEntityInstAttrCtx(),
				copyeeFetcher.getEstimatedVocabTypeTokenCount());

		final Object2IntMap<String> copyeeTrainingInstanceCounts = copyee.getTrainingInstanceCounts();
		trainingInstanceCounts = new Object2IntOpenHashMap<>(copyeeTrainingInstanceCounts);
		trainingInstanceCounts.defaultReturnValue(copyeeTrainingInstanceCounts.defaultReturnValue());
	}

	void addWordClassExamples(final String wordClass, final Stream<Entry<Instance, String>> instClassValues) {
		final Instances classInstances = fetchWordInstances(wordClass);
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
	}

	void addWordClassObservationCounts(final Object2IntMap<String> wordClassObservationCounts) {
		wordClassObservationCounts.object2IntEntrySet().forEach(wordClassObservationCount -> {
			final String wordClass = wordClassObservationCount.getKey();
			final Datum wordClassDatum = classData.get(wordClass);
			wordClassDatum.setObservationCount(
					wordClassDatum.getObservationCount() + wordClassObservationCount.getIntValue());
		});
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
		if (!(obj instanceof WordClassificationData)) {
			return false;
		}
		final WordClassificationData other = (WordClassificationData) obj;
		if (classInstancesFetcher == null) {
			if (other.classInstancesFetcher != null) {
				return false;
			}
		} else if (!classInstancesFetcher.equals(other.classInstancesFetcher)) {
			return false;
		}
		if (classData == null) {
			if (other.classData != null) {
				return false;
			}
		} else if (!classData.equals(other.classData)) {
			return false;
		}
		if (trainingInstanceCounts == null) {
			if (other.trainingInstanceCounts != null) {
				return false;
			}
		} else if (!trainingInstanceCounts.equals(other.trainingInstanceCounts)) {
			return false;
		}
		return true;
	}

	Instances fetchWordInstances(final String wordClass) {
		return classInstancesFetcher.apply(wordClass);
	}

	/**
	 * @return the classData
	 */
	public Map<String, Datum> getClassData() {
		return classData;
	}

	/**
	 * @return the classInstancesFetcher
	 */
	private WordClassInstancesFetcher getClassInstancesFetcher() {
		return classInstancesFetcher;
	}

	/**
	 * @return the trainingInstanceCounts
	 */
	public Object2IntMap<String> getTrainingInstanceCounts() {
		return trainingInstanceCounts;
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
		result = prime * result + (classInstancesFetcher == null ? 0 : classInstancesFetcher.hashCode());
		result = prime * result + (classData == null ? 0 : classData.hashCode());
		result = prime * result + (trainingInstanceCounts == null ? 0 : trainingInstanceCounts.hashCode());
		return result;
	}

	void incrementTrainingInstancesChangeCounts(Iterable<String> changedWordClasses) {
		changedWordClasses.forEach(wordClass -> {
			Datum datum = classData.get(wordClass);
			datum.incrementTrainingInstancesChangeCount();
		});
	}

}

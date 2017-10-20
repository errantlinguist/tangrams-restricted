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
package se.kth.speech.weka;

import java.util.function.Supplier;

import weka.classifiers.Classifier;
import weka.classifiers.UpdateableClassifier;
import weka.core.Capabilities;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 20 Oct 2017
 *
 */
public final class RetrainableClassifier implements Classifier, UpdateableClassifier {

	private final Supplier<? extends Classifier> classifierFactory;

	private Classifier decorated;

	private Instances trainingInsts;

	public RetrainableClassifier(final Supplier<? extends Classifier> classifierFactory,
			final Instances trainingInsts) {
		this.classifierFactory = classifierFactory;
		this.trainingInsts = trainingInsts;
		decorated = classifierFactory.get();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see weka.classifiers.Classifier#buildClassifier(weka.core.Instances)
	 */
	@Override
	public void buildClassifier(final Instances data) throws Exception {
		trainingInsts = data;
		final Classifier newClassifier = classifierFactory.get();
		newClassifier.buildClassifier(trainingInsts);
		decorated = newClassifier;

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see weka.classifiers.Classifier#classifyInstance(weka.core.Instance)
	 */
	@Override
	public double classifyInstance(final Instance instance) throws Exception {
		return decorated.classifyInstance(instance);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * weka.classifiers.Classifier#distributionForInstance(weka.core.Instance)
	 */
	@Override
	public double[] distributionForInstance(final Instance instance) throws Exception {
		return decorated.distributionForInstance(instance);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see weka.classifiers.Classifier#getCapabilities()
	 */
	@Override
	public Capabilities getCapabilities() {
		return decorated.getCapabilities();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see weka.classifiers.UpdateableClassifier#updateClassifier(weka.core.
	 * Instance)
	 */
	@Override
	public void updateClassifier(final Instance instance) throws Exception {
		trainingInsts.add(instance);
		final Classifier newClassifier = classifierFactory.get();
		newClassifier.buildClassifier(trainingInsts);
		decorated = newClassifier;
	}

}

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
package se.kth.speech.coin.tangrams.analysis.features.weka;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 15, 2017
 *
 */
public final class EntityInstanceAttributeContext {

	private static final List<String> CLASS_VALUES = Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString());

	/**
	 * @return the classValues
	 */
	public static List<String> getClassValues() {
		return CLASS_VALUES;
	}

	private static Attribute createClassAttr(final String classAttrName) {
		return new Attribute(classAttrName, CLASS_VALUES);
	}

	private final ArrayList<Attribute> attrs;

	private final Attribute classAttr;

	private final String classAttrName;

	private final InstanceFeatureExtractor<EntityFeature, EntityFeature.Extractor.Context> extractor;

	public EntityInstanceAttributeContext(
			final InstanceFeatureExtractor<EntityFeature, EntityFeature.Extractor.Context> extractor,
			final String classAttrName) {
		this.extractor = extractor;
		this.classAttrName = classAttrName;
		final Map<EntityFeature, Attribute> featureAttrs = extractor.getFeatureAttrs();
		attrs = new ArrayList<>(featureAttrs.size() + 1);
		attrs.addAll(featureAttrs.values());
		classAttr = createClassAttr(classAttrName);
		attrs.add(classAttr);
	}

	public Instance createInstance(final EntityFeature.Extractor.Context extractionContext, final Instances insts) {
		final Instance result = new DenseInstance(attrs.size());
		result.setDataset(insts);
		extractor.accept(result, extractionContext);
		return result;
	}

	/**
	 * @return the attrs
	 */
	public ArrayList<Attribute> getAttrs() {
		return attrs;
	}

	/**
	 * @return the classAttr
	 */
	public Attribute getClassAttr() {
		return classAttr;
	}

	/**
	 * @return the classAttrName
	 */
	public String getClassAttrName() {
		return classAttrName;
	}

	/**
	 * @return the extractor
	 */
	public InstanceFeatureExtractor<EntityFeature, EntityFeature.Extractor.Context> getExtractor() {
		return extractor;
	}

}

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
package se.kth.speech.coin.tangrams.analysis.features;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

import weka.core.Attribute;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 15, 2017
 *
 */
public final class EntityInstanceAttributeContext {

	private final EntityFeature.Extractor extractor;

	private final String classAttrName;

	private final ArrayList<Attribute> attrs;

	private final Attribute classAttr;

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

	public EntityInstanceAttributeContext(final EntityFeature.Extractor extractor, final String classAttrName) {
		this.extractor = extractor;
		this.classAttrName = classAttrName;
		final Map<EntityFeature, Attribute> featureAttrs = extractor.getFeatureAttrs();
		this.attrs = new ArrayList<>(featureAttrs.size() + 1);
		attrs.addAll(featureAttrs.values());
		this.classAttr = createClassAttr(classAttrName);
		attrs.add(classAttr);
	}

	/**
	 * @return the classAttr
	 */
	public static Attribute createClassAttr(String classAttrName) {
		return new Attribute(classAttrName, Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
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
	public EntityFeature.Extractor getExtractor() {
		return extractor;
	}

}

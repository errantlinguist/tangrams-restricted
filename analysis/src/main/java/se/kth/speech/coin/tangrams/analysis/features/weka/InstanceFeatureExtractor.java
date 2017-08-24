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

import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.BiConsumer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.features.FeatureExtractor;
import weka.core.Attribute;
import weka.core.Instance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 11, 2017
 *
 */
public final class InstanceFeatureExtractor<F, C> implements BiConsumer<Instance, C> { // NO_UCD (use default)

	private static final Logger LOGGER = LoggerFactory.getLogger(InstanceFeatureExtractor.class);

	private final FeatureExtractor<F, C> extractor;

	private final Map<F, Attribute> featureAttrs;

	InstanceFeatureExtractor(final FeatureExtractor<F, C> extractor, final Map<F, Attribute> featureAttrs) {
		this.extractor = extractor;
		this.featureAttrs = Collections.unmodifiableMap(featureAttrs);
	}

	@Override
	public void accept(final Instance vals, final C context) {
		for (final Entry<F, Attribute> featureAttr : featureAttrs.entrySet()) {
			final F feature = featureAttr.getKey();
			final Attribute attr = featureAttr.getValue();
			final Optional<Object> optVal = extractor.apply(feature, context);
			if (optVal.isPresent()) {
				final Object val = optVal.get();
				if (val instanceof Number) {
					vals.setValue(attr, ((Number) val).doubleValue());
				} else {
					vals.setValue(attr, val.toString());
				}
			} else {
				LOGGER.debug("No value found for feature \"{}\".", feature);
			}
		}
	}

	public Map<F, Attribute> getFeatureAttrs() {
		return featureAttrs;
	}

}

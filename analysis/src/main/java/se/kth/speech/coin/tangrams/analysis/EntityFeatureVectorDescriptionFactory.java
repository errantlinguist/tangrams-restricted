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
package se.kth.speech.coin.tangrams.analysis;

import java.util.Collection;
import java.util.Optional;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeatureExtractionContextFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Aug 2017
 *
 */
final class EntityFeatureVectorDescriptionFactory {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(EntityFeatureVectorDescriptionFactory.class);

	private final EntityFeature.Extractor entityFeatureExtractor;

	private final Collection<EntityFeature> entityFeaturesToDescribe;

	private final EntityFeatureExtractionContextFactory extractionContextFactory;

	private final String nullValueRepr;

	EntityFeatureVectorDescriptionFactory(final EntityFeature.Extractor entityFeatureExtractor,
			final Collection<EntityFeature> entityFeaturesToDescribe,
			final EntityFeatureExtractionContextFactory extractionContextFactory, final String nullValueRepr) {
		this.entityFeatureExtractor = entityFeatureExtractor;
		this.entityFeaturesToDescribe = entityFeaturesToDescribe;
		this.extractionContextFactory = extractionContextFactory;
		this.nullValueRepr = nullValueRepr;
	}

	Stream<String> createBlankFeatureValueReprs() {
		return entityFeaturesToDescribe.stream().map(feature -> nullValueRepr);
	}

	Stream<String> createFeatureValueReprs(final GameContext context) {
		final Optional<Integer> optSelectedEntityId = context.findLastSelectedEntityId();
		final Stream<String> result;
		if (optSelectedEntityId.isPresent()) {
			result = createFeatureValueReprs(context, optSelectedEntityId.get());
		} else {
			result = createBlankFeatureValueReprs();
		}
		return result;
	}

	Stream<String> createFeatureValueReprs(final GameContext context, final int entityId) {
		LOGGER.debug("Getting feature value representation for entity {}.", entityId);
		final EntityFeature.Extractor.Context extractionContext = extractionContextFactory.apply(context, entityId);
		final Stream<Optional<Object>> featureVals = entityFeaturesToDescribe.stream()
				.map(feature -> entityFeatureExtractor.apply(feature, extractionContext));
		return featureVals.map(opt -> opt.map(Object::toString).orElse(nullValueRepr));
	}

	/**
	 * @return the entityFeaturesToDescribe
	 */
	Collection<EntityFeature> getEntityFeaturesToDescribe() {
		return entityFeaturesToDescribe;
	}

}

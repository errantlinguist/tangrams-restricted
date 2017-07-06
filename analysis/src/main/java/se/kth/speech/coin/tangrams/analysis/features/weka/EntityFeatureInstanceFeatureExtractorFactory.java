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

import java.net.URL;
import java.util.ArrayList;
import java.util.Map;
import java.util.NavigableMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.FactoryBean;

import se.kth.speech.coin.tangrams.analysis.features.EntityFeature;
import se.kth.speech.coin.tangrams.analysis.features.EntityFeature.Extractor.Context;
import se.kth.speech.coin.tangrams.content.IconImages;
import weka.core.Attribute;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 6, 2017
 *
 */
public final class EntityFeatureInstanceFeatureExtractorFactory
		implements FactoryBean<InstanceFeatureExtractor<EntityFeature, EntityFeature.Extractor.Context>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(EntityFeatureInstanceFeatureExtractorFactory.class);

	private static InstanceFeatureExtractor<EntityFeature, EntityFeature.Extractor.Context> create() {
		LOGGER.info("Creating new Instance feature extractor.");
		final NavigableMap<String, URL> namedImgResources = IconImages.createImageResourceMap();
		LOGGER.info("Creating named image resource map of size {}.", namedImgResources.size());
		final EntityFeature.Extractor fExtr = new EntityFeature.Extractor();
		final Map<EntityFeature, Attribute> fAttrs = EntityFeature.Extractor
				.createFeatureAttrMap(new ArrayList<>(namedImgResources.keySet()));
		return new InstanceFeatureExtractor<>(fExtr, fAttrs);
	}

	private InstanceFeatureExtractor<EntityFeature, EntityFeature.Extractor.Context> inst = null;

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 */
	@Override
	public InstanceFeatureExtractor<EntityFeature, Context> getObject() throws Exception {
		if (inst == null) {
			synchronized (this) {
				if (inst == null) {
					inst = create();
				}
			}
		}
		return inst;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.beans.factory.FactoryBean#getObjectType()
	 */
	@Override
	public Class<?> getObjectType() {
		return InstanceFeatureExtractor.class;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.beans.factory.FactoryBean#isSingleton()
	 */
	@Override
	public boolean isSingleton() {
		return true;
	}
}

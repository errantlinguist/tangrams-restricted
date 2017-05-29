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
package se.kth.speech.nlp;

import java.util.Collection;
import java.util.Set;

import org.springframework.beans.factory.FactoryBean;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 24 May 2017
 *
 */
public final class SnowballPorter2EnglishStopwordSetFactory implements FactoryBean<Set<String>> {

	private Collection<SnowballPorter2EnglishStopwords.Variant> variantsToUnify;

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 */
	@Override
	public Set<String> getObject() {
		return SnowballPorter2EnglishStopwords.loadStopwordSet(variantsToUnify);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.beans.factory.FactoryBean#getObjectType()
	 */
	@Override
	public Class<?> getObjectType() {
		return Set.class;
	}

	/**
	 * @return the variantsToUnify
	 */
	public Collection<SnowballPorter2EnglishStopwords.Variant> getVariantsToUnify() {
		return variantsToUnify;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.beans.factory.FactoryBean#isSingleton()
	 */
	@Override
	public boolean isSingleton() {
		return false;
	}

	/**
	 * @param variantsToUnify
	 *            the variantsToUnify to set
	 */
	public void setVariantsToUnify(final Collection<SnowballPorter2EnglishStopwords.Variant> variantsToUnify) {
		this.variantsToUnify = variantsToUnify;
	}

}

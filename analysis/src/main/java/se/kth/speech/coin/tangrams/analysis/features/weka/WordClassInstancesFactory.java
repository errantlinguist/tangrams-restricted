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
package se.kth.speech.coin.tangrams.analysis.features.weka;

import java.util.function.BiFunction;

import javax.inject.Inject;

import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 May 2017
 *
 */
public final class WordClassInstancesFactory implements BiFunction<String, Integer, Instances> {

	@Inject
	private EntityInstanceAttributeContext entInstAttrCtx;

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.function.IntFunction#apply(int)
	 */
	@Override
	public Instances apply(String relName, final Integer initialCapacity) {
		final Instances result = new Instances(relName, entInstAttrCtx.getAttrs(), initialCapacity);
		result.setClass(entInstAttrCtx.getClassAttr());
		return result;
	}

}
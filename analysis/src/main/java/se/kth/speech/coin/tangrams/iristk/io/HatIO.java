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
package se.kth.speech.coin.tangrams.iristk.io;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Jul 3, 2017
 *
 */
public final class HatIO {

	private static Reference<JAXBContext> JC = new SoftReference<>(null);

	public static JAXBContext fetchContext() {
		JAXBContext result = JC.get();
		if (result == null) {
			synchronized (HatIO.class) {
				result = JC.get();
				if (result == null) {
					try {
						result = JAXBContext.newInstance("se.kth.speech.hat.xsd");
						JC = new SoftReference<>(result);
					} catch (final JAXBException e) {
						throw new RuntimeException(e);
					}
				}
			}
		}
		return result;
	}

	private HatIO() {
	}

}

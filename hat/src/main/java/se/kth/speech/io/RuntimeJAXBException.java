/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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
package se.kth.speech.io;

import javax.xml.bind.JAXBException;

public class RuntimeJAXBException extends RuntimeException {

	/**
	 *
	 */
	private static final long serialVersionUID = 8911045968943261899L;

	public RuntimeJAXBException() {
		super();
	}

	public RuntimeJAXBException(final JAXBException cause) {
		super(cause);
	}

	public RuntimeJAXBException(final String message, final JAXBException cause) { // NO_UCD (unused code)
		super(message, cause);
	}

	public RuntimeJAXBException(final String message, final JAXBException cause, final boolean enableSuppression, // NO_UCD (unused code)
			final boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
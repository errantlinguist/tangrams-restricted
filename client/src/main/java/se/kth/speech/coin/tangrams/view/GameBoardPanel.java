/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.view;

import java.util.function.Function;

import org.apache.batik.swing.JSVGCanvas;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.svg.SVGDocument;

import se.kth.speech.coin.tangrams.content.ImageDatum;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanel extends JSVGCanvas {

	/**
	 *
	 */
	private static final long serialVersionUID = 6258829324465894025L;

	/**
	 * @param coordOccupantImageFactory
	 */
	public GameBoardPanel(final Function<? super Integer, ? extends ImageDatum> coordOccupantImageFactory) {
		setDocumentState(ALWAYS_DYNAMIC);
		setURI(coordOccupantImageFactory.apply(1).getResourceLoc().toString());
		// TODO Auto-generated constructor stub
	}

}

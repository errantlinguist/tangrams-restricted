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

import java.awt.Canvas;
import java.awt.Component;
import java.awt.Image;
import java.util.function.Function;

import javax.swing.JPanel;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanel<T> extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6258829324465894025L;
	
	GameBoardPanel() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param coordOccupantImageFactory
	 */
	public GameBoardPanel(Function<? super T, ? extends Image> coordOccupantImageFactory) {
		// TODO Auto-generated constructor stub
	}

}

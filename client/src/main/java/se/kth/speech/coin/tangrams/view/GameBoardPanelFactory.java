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

import java.awt.Image;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanelFactory {
	
	GameBoardPanelFactory(){
		
	}
	
	private static double RATIO_TOLERANCE = 0.05;
	
	private void getImageCoordinateSize(int width, int height){
		final boolean isPortrait;
		final double ratio;
		if (width < height){
			isPortrait = true;
			ratio = height / (double) width;
		} else {
			isPortrait = false;
			ratio = width / (double) height;
		}
		long wholePart = (long) ratio;
		double fractionPart = ratio - wholePart;
		
	}

}

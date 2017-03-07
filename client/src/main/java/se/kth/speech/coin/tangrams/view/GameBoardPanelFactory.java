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
import java.util.Map;
import java.util.function.Function;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameBoardPanelFactory implements Function<Map<? super Integer, ? extends Image>, GameBoardPanel> {
	
	GameBoardPanelFactory(){
		
	}
	
	private static double RATIO_TOLERANCE = 0.05;
	
	/**
	 * @see <a href="http://stackoverflow.com/a/4009247/1391325">StackOverflow</a>
	 * @param a
	 * @param b
	 * @return
	 */
	public static int GCD(int a, int b) {
		   if (b==0) return a;
		   return GCD(b,a%b);
		}
	
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

	/* (non-Javadoc)
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public GameBoardPanel apply(Map<? super Integer, ? extends Image> t) {
		// TODO Auto-generated method stub
		return null;
	}

}

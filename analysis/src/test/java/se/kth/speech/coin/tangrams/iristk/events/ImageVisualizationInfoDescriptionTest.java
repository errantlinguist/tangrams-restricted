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
package se.kth.speech.coin.tangrams.iristk.events;

import org.junit.Assert;
import org.junit.Test;

/**
 * Created to investigate a case of {@link java.lang.NoClassDefFoundError} when
 * instantiating
 * {@link se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription}.
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since Jul 5, 2017
 *
 */
public final class ImageVisualizationInfoDescriptionTest {

	/**
	 * Test method for
	 * {@link se.kth.speech.coin.tangrams.iristk.events.ImageVisualizationInfoDescription#ImageVisualizationInfoDescription()}.
	 */
	@Test
	public void testImageVisualizationInfoDescription() {
		final ImageVisualizationInfoDescription testInst = new ImageVisualizationInfoDescription();
		Assert.assertNull(testInst.getData());
		Assert.assertEquals(testInst.getUniqueImgResourceCount(), 0);
	}

}

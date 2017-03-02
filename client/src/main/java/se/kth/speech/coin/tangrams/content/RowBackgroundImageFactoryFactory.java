/*
 *  This file is part of se.kth.speech.coin.tangrams.client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.content;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Collections;
import java.util.List;
import java.util.function.IntFunction;

import javax.imageio.ImageIO;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.Lists;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 21 Feb 2017
 *
 */
public final class RowBackgroundImageFactoryFactory implements IntFunction<IntFunction<BufferedImage>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RowBackgroundImageFactoryFactory.class);

	private static List<URL> createRowBackgroundImageLocatorList(final int rowCount) {
		// final String backgroundPrefix = ImageType.BACKGROUND.getDirLocator()
		// + "/";
		// final List<URL> backgroundImgLocs = Arrays.asList(
		// // @formatter:off
		// RowBackgroundImageFactoryFactory.class.getResource(backgroundPrefix +
		// "diag1px.png"),
		// null,
		// RowBackgroundImageFactoryFactory.class.getResource(backgroundPrefix +
		// "diag1px.png"));
		// // @formatter:on
		// TODO: Decide what kinds of backgrounds to have (if any)
		final List<URL> backgroundImgLocs = Collections.singletonList(null);
		return Lists.createGreatestDivisorList(backgroundImgLocs, rowCount);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.function.IntFunction#apply(int)
	 */
	@Override
	public IntFunction<BufferedImage> apply(final int rowCount) {
		final List<URL> rowBackgroundImgLocs = createRowBackgroundImageLocatorList(rowCount);
		return rowIdx -> {
			final URL rowBackgroundImgLoc = rowBackgroundImgLocs.get(rowIdx);
			try {
				return rowBackgroundImgLoc == null ? null : ImageIO.read(rowBackgroundImgLoc);
			} catch (final IOException e) {
				// Should never happen in a production environment
				LOGGER.error(String.format(
						"An I/O exception occurred while loading image at \"%s\"; Re-throwing as an unchecked exception.",
						rowBackgroundImgLoc), e);
				throw new UncheckedIOException(e);
			}
		};
	}

}

/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams.view;

import java.awt.Component;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.ExecutorService;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

import javax.imageio.ImageIO;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.awt.ComponentImageCapture;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Jan 2017
 *
 */
final class ScreenshotLogger implements BiConsumer<Component, String> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ScreenshotLogger.class);

	private final ExecutorService executor;

	private final Supplier<? extends Path> outdirPathSupplier;

	private final Supplier<String> playerIdGetter;

	ScreenshotLogger(final Supplier<? extends Path> outdirPathSupplier, final Supplier<String> playerIdGetter,
			final ExecutorService executor) {
		this.outdirPathSupplier = outdirPathSupplier;
		this.playerIdGetter = playerIdGetter;
		this.executor = executor;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.BiConsumer#accept(java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public void accept(final Component view, final String filenamePrefix) {
		final BufferedImage img = ComponentImageCapture.createScreenshot(view);
		executor.submit(() -> {
			try {
				final Path outdir = outdirPathSupplier.get();
				Files.createDirectories(outdir);
				final String playerId = playerIdGetter.get();
				LOGGER.debug("Creating and saving screenshot \"{}\", taken by \"{}\".", filenamePrefix, playerId);
				final String outfileName = filenamePrefix + "-" + playerIdGetter.get() + ".png";
				try (final OutputStream os = Files.newOutputStream(outdir.resolve(outfileName))) {
					// write the image as a PNG
					ImageIO.write(img, "png", os);
				}

			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
		});
	}

}

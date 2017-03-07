/*
 *  This file is part of client.
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

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.NavigableMap;
import java.util.function.Function;

import org.apache.batik.transcoder.SVGAbstractTranscoder;
import org.apache.batik.transcoder.TranscoderException;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;
import org.apache.batik.transcoder.image.PNGTranscoder;

import se.kth.speech.FilenameBaseSplitter;

/**
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class IconImagePNGTranscoder {

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/q/32721467/1391325">StackOverflow</a>
	 * @param doc
	 * @throws TranscoderException
	 * @throws IOException
	 */
	public static void convertSVGToPNG(final String inputUri, final Path outpath)
			throws TranscoderException, IOException {
		final ByteArrayOutputStream resultByteStream = new ByteArrayOutputStream();
		final TranscoderInput transcoderInput = new TranscoderInput(inputUri);
		final TranscoderOutput transcoderOutput = new TranscoderOutput(resultByteStream);

		final PNGTranscoder pngTranscoder = new PNGTranscoder();
		pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_HEIGHT, 256f);
		pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_WIDTH, 256f);
		pngTranscoder.transcode(transcoderInput, transcoderOutput);

		try (OutputStream os = new BufferedOutputStream(
				Files.newOutputStream(outpath, StandardOpenOption.CREATE, StandardOpenOption.APPEND))) {
			resultByteStream.writeTo(os);
			// writer.flush();
		}

	}

	public static void main(final String[] args) throws TranscoderException, IOException, URISyntaxException {
		final NavigableMap<String, URL> files = IconImages.getIconImageResources();
		final Function<String, String> outfilePathFactory = new FilenameBaseSplitter().andThen(base -> base + ".png");
		for (final URL resourceLoc : files.values()) {
			final String inputUri = resourceLoc.toString();
			System.out.print(inputUri + " > ");
			final String outputUriStr = outfilePathFactory.apply(inputUri);
			final URI outputUri = URI.create(outputUriStr);
			final Path outputPath = Paths.get(outputUri);
			convertSVGToPNG(resourceLoc.toString(), outputPath);
			System.out.println(outputPath);
		}
	}

}

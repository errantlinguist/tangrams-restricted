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
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.NavigableMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.batik.anim.dom.SAXSVGDocumentFactory;
import org.apache.batik.bridge.UserAgent;
import org.apache.batik.transcoder.SVGAbstractTranscoder;
import org.apache.batik.transcoder.TranscoderException;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;
import org.apache.batik.transcoder.image.PNGTranscoder;
import org.apache.batik.util.XMLResourceDescriptor;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import se.kth.speech.FilenameBaseSplitter;

/**
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class IconImagePNGTranscoder {

	private static final FilenameBaseSplitter FILENAME_BASE_SPLITTER = new FilenameBaseSplitter();

	private static final Pattern LENGTH_MEASUREMENT_PATTERN = Pattern.compile("(\\d+(?:\\.\\d+)?)(\\S*)");

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/q/32721467/1391325">StackOverflow</a>
	 * @param doc
	 * @throws TranscoderException
	 * @throws IOException
	 */
	public static void convertSVGToPNG(final String inputUri, final Path outpath)
			throws TranscoderException, IOException {
		final Document doc = createSVGDocument(inputUri);
		final ByteArrayOutputStream resultByteStream = new ByteArrayOutputStream();
		final TranscoderInput transcoderInput = new TranscoderInput(doc);
		final TranscoderOutput transcoderOutput = new TranscoderOutput(resultByteStream);

		final PNGTranscoder pngTranscoder = new PNGTranscoder();
		final UserAgent userAgent = pngTranscoder.getUserAgent();

		final Matcher lengthValMatcher = LENGTH_MEASUREMENT_PATTERN.matcher("");
		final NodeList svgNodes = doc.getElementsByTagName("svg");
		float maxWidth = -1;
		float maxHeight = -1;
		for (int i = 0; i < svgNodes.getLength(); ++i) {
			final Node svgNode = svgNodes.item(i);
			final NamedNodeMap svgNodeAttrs = svgNode.getAttributes();
			{
				final Node svgWidthAttrNode = svgNodeAttrs.getNamedItem("width");
				lengthValMatcher.reset(svgWidthAttrNode.getTextContent());
				if (lengthValMatcher.matches()) {
					final float width = getNormalizedLength(lengthValMatcher, userAgent);
					maxWidth = Math.max(maxWidth, width);
				}
			}
			{
				final Node svgHeightAttrNode = svgNodeAttrs.getNamedItem("height");
				lengthValMatcher.reset(svgHeightAttrNode.getTextContent());
				if (lengthValMatcher.matches()) {
					final float height = getNormalizedLength(lengthValMatcher, userAgent);
					maxHeight = Math.max(maxHeight, height);
				}
			}

		}

		pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_WIDTH, Float.valueOf(maxWidth));
		pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_HEIGHT, Float.valueOf(maxHeight));
		pngTranscoder.transcode(transcoderInput, transcoderOutput);

		try (OutputStream os = new BufferedOutputStream(
				Files.newOutputStream(outpath, StandardOpenOption.CREATE, StandardOpenOption.APPEND))) {
			resultByteStream.writeTo(os);
			// writer.flush();
		}

	}

	public static void main(final String[] args) throws TranscoderException, IOException, URISyntaxException {
		if (args.length < 1) {
			System.err.println(String.format("Usage: %s <outputDir>", IconImagePNGTranscoder.class.getName()));
			System.exit(64);
		} else {
			// E.g.
			// <src/main/resources/se/kth/speech/coin/tangrams/content/images/icons/>
			final Path outputDir = Paths.get(args[0]);
			final NavigableMap<String, URL> files = IconImages.getIconImageResources();
			for (final URL resourceLoc : files.values()) {
				final Path inputPath = Paths.get(resourceLoc.toURI());
				System.out.print(inputPath + " > ");
				final Path outputPath = createOutputFilePath(inputPath, outputDir);
				convertSVGToPNG(resourceLoc.toString(), outputPath);
				System.out.println(outputPath);
			}
		}

	}

	private static Path createOutputFilePath(final Path inputPath, final Path outputDir) {
		final Path inputFilenamePath = inputPath.getName(inputPath.getNameCount() - 1);
		final String inputFilename = inputFilenamePath.toString();
		final String filenameBase = FILENAME_BASE_SPLITTER.apply(inputFilename);
		final String outputFilename = filenameBase + ".png";
		return outputDir.resolve(outputFilename);
	}

	/**
	 * Use the SAXSVGDocumentFactory to parse the given URI into a DOM.
	 *
	 * @param uri
	 *            The path to the SVG file to read.
	 * @return A Document instance that represents the SVG file.
	 * @throws IOException
	 *             The file could not be read.
	 */
	private static Document createSVGDocument(final String uri) throws IOException {
		final String parser = XMLResourceDescriptor.getXMLParserClassName();
		final SAXSVGDocumentFactory factory = new SAXSVGDocumentFactory(parser);
		return factory.createDocument(uri);
	}

	private static float getNormalizedLength(final Matcher lengthValMatcher, final UserAgent userAgent) {
		float result = Float.parseFloat(lengthValMatcher.group(1));
		if (lengthValMatcher.groupCount() > 1) {
			final String measurement = lengthValMatcher.group(2);
			if (measurement.equalsIgnoreCase("mm")) {
				result = result / userAgent.getPixelUnitToMillimeter();
			}
		}
		return result;
	}

}

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
package se.kth.speech.coin.tangrams.content;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Map.Entry;
import java.util.NavigableMap;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.WindowConstants;

import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.swing.svg.JSVGComponent;
import org.apache.batik.swing.svg.SVGDocumentLoaderAdapter;
import org.apache.batik.swing.svg.SVGDocumentLoaderEvent;
import org.apache.batik.transcoder.SVGAbstractTranscoder;
import org.apache.batik.transcoder.TranscoderException;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;
import org.apache.batik.transcoder.image.PNGTranscoder;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.svg.SVGDocument;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 7 Mar 2017
 *
 */
public final class IconImageViewingTest {

	public static BufferedImage convertSVGToPNG(Document doc) throws TranscoderException, IOException {
		ByteArrayOutputStream resultByteStream = new ByteArrayOutputStream();

		TranscoderInput transcoderInput = new TranscoderInput(doc);
		TranscoderOutput transcoderOutput = new TranscoderOutput(resultByteStream);

		PNGTranscoder pngTranscoder = new PNGTranscoder();
		pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_HEIGHT, 256f);
		pngTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_WIDTH, 256f);
		pngTranscoder.transcode(transcoderInput, transcoderOutput);

		resultByteStream.flush();

		return ImageIO.read(new ByteArrayInputStream(resultByteStream.toByteArray()));
	}

	public static void main(final String[] args) {
		final JFrame f = new JFrame("Image viewer");
		// f.setLayout(new BorderLayout());
		// f.setPreferredSize(new Dimension(1000,100));
		final JSVGCanvas canvas = new JSVGCanvas();
		// canvas.setPreferredSize(new Dimension(1000,100));
		// canvas.set
		// canvas.setLayout(new BorderLayout());
		canvas.setDocumentState(JSVGComponent.ALWAYS_DYNAMIC);
		f.add(canvas);
		EventQueue.invokeLater(() -> {
			final NavigableMap<String, URL> files = IconImages.getIconImageResources();
			final Entry<String, URL> first = files.firstEntry();
			final URL firstImgResourceLoc = first.getValue();
			canvas.addSVGDocumentLoaderListener(new SVGDocumentLoaderAdapter() {

				/*
				 * (non-Javadoc)
				 * 
				 * @see org.apache.batik.swing.svg.SVGDocumentLoaderAdapter#
				 * documentLoadingCompleted(org.apache.batik.swing.svg.
				 * SVGDocumentLoaderEvent)
				 */
				@Override
				public void documentLoadingCompleted(final SVGDocumentLoaderEvent e) {
					final SVGDocument doc = canvas.getSVGDocument();

					final NodeList pathNodes = doc.getElementsByTagName("path");
					for (int pathNodeIdx = 0; pathNodeIdx < pathNodes.getLength(); ++pathNodeIdx) {
						final Node pathNode = pathNodes.item(pathNodeIdx);
						final NamedNodeMap pathNodeAttrs = pathNode.getAttributes();
						final Node styleAttrNode = pathNodeAttrs.getNamedItem("style");
						final String styleStr = styleAttrNode.getTextContent();
						// System.out.println(styleStr);
						styleAttrNode.setTextContent(styleStr + ";fill:purple");
					}
					NodeList svgNodes = doc.getElementsByTagName("svg");
					for (int svgNodeIdx = 0; svgNodeIdx < svgNodes.getLength(); ++svgNodeIdx) {
						Node svgNode = svgNodes.item(svgNodeIdx);
						NamedNodeMap svgAttrs = svgNode.getAttributes();
						Node widthAttrNode = svgAttrs.getNamedItem("width");
						String width = widthAttrNode.getTextContent();
						System.out.println(width);
						widthAttrNode.setTextContent("200mm");
						System.out.println(widthAttrNode.getTextContent());
						Node heightAttrNode = svgAttrs.getNamedItem("height");
						String height = heightAttrNode.getTextContent();
						System.out.println(height);
					}
					
					
					EventQueue.invokeLater(()-> {
						JFrame conv = new JFrame("Converted");
						JSVGCanvas convCanvas = new JSVGCanvas();
						conv.add(convCanvas);
						convCanvas.setDocumentState(JSVGComponent.ALWAYS_DYNAMIC);
						convCanvas.setSVGDocument(doc);
						convCanvas.addSVGDocumentLoaderListener(new SVGDocumentLoaderAdapter(){
							
						});
						conv.pack();
//						conv.setLocation(null);
						conv.setVisible(true);
					});

//					try {
//						BufferedImage img = convertSVGToPNG(doc);
//						EventQueue.invokeLater(() -> {
//							JFrame c = new JFrame("Converted");
//							c.add(new JLabel(new ImageIcon(img)));
//							c.pack();
//							c.setLocationByPlatform(true);
//							c.setVisible(true);
//						});
//					} catch (IOException e1) {
//						throw new UncheckedIOException(e1);
//					} catch (TranscoderException e1) {
//						throw new RuntimeException(e1);
//					}

					// canvas.setSVGDocument(doc);
					// f.invalidate();
					// canvas.repaint();
				}

			});
			canvas.setURI(firstImgResourceLoc.toString());

			f.pack();
			f.setLocationByPlatform(true);
			f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
			f.setVisible(true);
		});
	}

}

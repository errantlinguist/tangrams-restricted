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

import java.awt.EventQueue;
import java.net.URL;
import java.util.Map.Entry;
import java.util.NavigableMap;

import javax.swing.JFrame;
import javax.swing.WindowConstants;

import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.swing.svg.JSVGComponent;
import org.apache.batik.swing.svg.SVGDocumentLoaderAdapter;
import org.apache.batik.swing.svg.SVGDocumentLoaderEvent;
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

	public static void main(final String[] args) {
		final JFrame f = new JFrame("Image viewer");
		final JSVGCanvas canvas = new JSVGCanvas();
//		canvas.setDocumentState(JSVGComponent.ALWAYS_DYNAMIC);
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
//					NodeList svgNodes = doc.getElementsByTagName("svg");
//					for (int svgNodeIdx = 0; svgNodeIdx < svgNodes.getLength(); ++svgNodeIdx){
//						Node svgNode = svgNodes.item(svgNodeIdx);
//						NamedNodeMap svgAttrs = svgNode.getAttributes();
//						Node widthAttrNode = svgAttrs.getNamedItem("width");
//						String width = widthAttrNode.getTextContent();
//						System.out.println(width);
//						widthAttrNode.setTextContent("200mm");
//						System.out.println(widthAttrNode.getTextContent());
//						f.validate();
//						Node heightAttrNode = svgAttrs.getNamedItem("height");
//						String height = heightAttrNode.getTextContent();
//						System.out.println(height);
//					
//					}
					
					final NodeList pathNodes = doc.getElementsByTagName("path");
					for (int pathNodeIdx = 0; pathNodeIdx < pathNodes.getLength(); ++pathNodeIdx) {
						final Node pathNode = pathNodes.item(pathNodeIdx);
						final NamedNodeMap pathNodeAttrs = pathNode.getAttributes();
						final Node styleAttrNode = pathNodeAttrs.getNamedItem("style");
						final String styleStr = styleAttrNode.getTextContent();
//						System.out.println(styleStr);
						styleAttrNode.setTextContent(styleStr + ";fill:purple");
					}		
					
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

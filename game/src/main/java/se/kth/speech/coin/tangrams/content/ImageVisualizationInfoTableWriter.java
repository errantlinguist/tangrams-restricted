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

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map.Entry;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 17 Mar 2017
 *
 */
public final class ImageVisualizationInfoTableWriter {

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

	private final ImageVisualizationInfoTableRowWriter rowWriter;

	private final Writer writer;

	public ImageVisualizationInfoTableWriter(final Writer writer) {
		this(writer, new ImageVisualizationInfoTableRowWriter(writer));
	}

	ImageVisualizationInfoTableWriter(final Writer writer,
			final ImageVisualizationInfoTableRowWriter rowWriter) {
		this.writer = writer;
		this.rowWriter = rowWriter;
	}

	public void write(final Iterator<? extends Entry<?, ImageVisualizationInfo.Datum>> imgVisualizationInfoDataIter)
			throws IOException {
		rowWriter.writeHeader();
		while (imgVisualizationInfoDataIter.hasNext()) {
			writer.append(TABLE_STRING_REPR_ROW_DELIMITER);
			final Entry<?, ImageVisualizationInfo.Datum> datumForId = imgVisualizationInfoDataIter.next();
			rowWriter.write(datumForId.getKey(), datumForId.getValue());
		}
	}

}

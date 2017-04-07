/*
 *  This file is part of analysis.
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
package se.kth.speech.coin.tangrams;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.SpatialMatrix;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.content.ImageVisualizationInfo;
import se.kth.speech.coin.tangrams.iristk.io.LoggingFormats;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 5, 2017
 *
 */
public final class ModelFeatureExtractor {

	private static final Logger LOGGER = LoggerFactory.getLogger(ModelFeatureExtractor.class);

	public static void main(final String[] args) throws IOException {
		if (args.length < 1) {
			final String usageMsg = String.format("Usage: %s INFILE", ModelFeatureExtractor.class.getName());
			throw new IllegalArgumentException(usageMsg);
		} else {
			final Path inpath = Paths.get(args[1]);
			try (Stream<String> lines = Files.lines(inpath, LoggingFormats.ENCODING)) {
			}
		}
	}

	/**
	 *
	 */
	public ModelFeatureExtractor() {
		// TODO Auto-generated constructor stub
	}

	public void extract(final ImageVisualizationInfo imgVizInfo, final SpatialMatrix<Integer> model) {
		final Map<Integer, SpatialRegion> piecePlacements = model.getElementPlacements().getElementMinimalRegions();
		for (final Entry<Integer, SpatialRegion> piecePlacement : piecePlacements.entrySet()) {
			final ImageVisualizationInfo.Datum imgVizInfoDatum = imgVizInfo.getData().get(piecePlacement.getKey());
		}
	}

	public void readLogfile() {

	}

}

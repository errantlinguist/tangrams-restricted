/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
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
package se.kth.speech.coin.tangrams.analysis.vocab;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.NavigableSet;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.hat.xsd.Annotation;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 *
 */
public final class HATWordListFactory implements Function<Stream<Path>, NavigableSet<String>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(HATWordListFactory.class);

	private final Collector<Annotation, ?, NavigableSet<String>> collector;

	private final Supplier<? extends Unmarshaller> unmarshallerSupplier;
	
	/**
	 *
	 */
	public HATWordListFactory(final Collector<Annotation, ?, NavigableSet<String>> collector,
			final Supplier<? extends Unmarshaller> unmarshallerSupplier) {
		this.collector = collector;
		this.unmarshallerSupplier = unmarshallerSupplier;
	}

	@Override
	public NavigableSet<String> apply(final Stream<Path> inpaths) {
		final Stream<Path> xmlFilePaths = inpaths.filter(Files::isRegularFile).filter(inpath -> {
			boolean shouldBeParsed = false;
			try {
				final String contentType = Files.probeContentType(inpath);
				shouldBeParsed = contentType != null && contentType.endsWith("/xml");
			} catch (final IOException e) {
				LOGGER.warn(
						"A(n) {} occurred while probing the content type of \"{}\"; Skipping file.",
						new Object[] { e.getClass().getSimpleName(), inpath }, e);
				shouldBeParsed = true;
			}
			return shouldBeParsed;
		});
		final Stream<Annotation> annots = xmlFilePaths.map(Path::toFile).flatMap(infile -> {
			Stream<Annotation> annot = Stream.empty();
			LOGGER.info("Reading \"{}\".", infile);
			try {
				annot = Stream.of((Annotation) unmarshallerSupplier.get().unmarshal(infile));
			} catch (final JAXBException e) {
				LOGGER.warn("A(n) {} occurred while reading \"{}\"; Skipping.",
						new Object[] { e.getClass().getSimpleName(), infile }, e);
			}
			return annot;
		});
		return annots.collect(collector);
	}

}

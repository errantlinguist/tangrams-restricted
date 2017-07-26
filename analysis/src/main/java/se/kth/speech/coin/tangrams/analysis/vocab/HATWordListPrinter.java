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
package se.kth.speech.coin.tangrams.analysis.vocab;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Iterator;
import java.util.NavigableSet;
import java.util.function.Supplier;
import java.util.stream.Stream;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.MissingOptionException;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.CLIParameters;
import se.kth.speech.coin.tangrams.iristk.io.HatIO;
import se.kth.speech.hat.xsd.Annotation;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 5, 2017
 *
 */
public final class HATWordListPrinter {

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		OUTFILE("o") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("outpath").desc("The path of the word list file to write.")
						.hasArg().argName("path").type(File.class).build();
			}
		};

		private static final Options OPTIONS = createOptions();

		private static Options createOptions() {
			final Options result = new Options();
			Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
			return result;
		}

		private static void printHelp() {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(HATWordListPrinter.class.getSimpleName() + " INPATHS...", OPTIONS);
		}

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

	private static final Logger LOGGER = LoggerFactory.getLogger(HATWordListPrinter.class);

	private static final ThreadLocal<Unmarshaller> UNMARSHALLER = new ThreadLocal<Unmarshaller>() {
		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.ThreadLocal#initialValue()
		 */
		@Override
		protected Unmarshaller initialValue() {
			try {
				return HatIO.fetchContext().createUnmarshaller();
			} catch (final JAXBException e) {
				throw new RuntimeException(e);
			}
		}
	};

	public static void main(final String[] args) throws IOException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(Parameter.OPTIONS, args);
			if (cl.hasOption(Parameter.HELP.optName)) {
				Parameter.printHelp();
			} else {
				final Path[] inpaths = cl.getArgList().stream().map(String::trim).filter(path -> !path.isEmpty())
						.map(Paths::get).toArray(Path[]::new);
				switch (inpaths.length) {
				case 0: {
					throw new MissingOptionException("No input path specified.");
				}
				case 1: {
					final Path inpath = inpaths[0];
					try (final PrintWriter out = CLIParameters
							.parseOutpath((File) cl.getParsedOptionValue(Parameter.OUTFILE.optName))) {
						run(inpath, out);
					}
					break;
				}
				default: {
					throw new IllegalArgumentException("No support for multiple inpaths (yet).");
				}
				}
			}
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			Parameter.printHelp();
		}
	}

	private static Stream<Annotation> parseAnnotations(final Stream<Path> inpaths) {
		final Stream<Path> xmlFilePaths = inpaths.filter(Files::isRegularFile).filter(inpath -> {
			boolean shouldBeParsed = false;
			try {
				final String contentType = Files.probeContentType(inpath);
				shouldBeParsed = contentType != null && contentType.endsWith("/xml");
			} catch (final IOException e) {
				LOGGER.warn("A(n) {} occurred while probing the content type of \"{}\"; Skipping file.",
						new Object[] { e.getClass().getSimpleName(), inpath }, e);
				shouldBeParsed = true;
			}
			return shouldBeParsed;
		});
		return xmlFilePaths.map(Path::toFile).flatMap(infile -> {
			Stream<Annotation> annot = Stream.empty();
			LOGGER.info("Reading \"{}\".", infile);
			try {
				annot = Stream.of((Annotation) UNMARSHALLER.get().unmarshal(infile));
			} catch (final JAXBException e) {
				LOGGER.warn("A(n) {} occurred while reading \"{}\"; Skipping.",
						new Object[] { e.getClass().getSimpleName(), infile }, e);
			}
			return annot;
		});
	}

	/**
	 * @param infile
	 * @param out
	 * @throws IOException
	 */
	private static void run(final Path infile, final PrintWriter out) throws IOException {
		LOGGER.info("Reading annotations from \"{}\".", infile);
		final Stream<Path> inpaths = Files.walk(infile, FileVisitOption.FOLLOW_LINKS);
		final Stream<Annotation> annots = parseAnnotations(inpaths);
		final NavigableSet<String> wordList = annots.collect(new AnnotationVocabularyCollector());

		final Iterator<String> wordIter = wordList.iterator();
		if (wordIter.hasNext()) {
			final String first = wordIter.next();
			out.print(first);
			while (wordIter.hasNext()) {
				out.print(System.lineSeparator());
				final String next = wordIter.next();
				out.print(next);
			}
		}
	}

}

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
package se.kth.speech.coin.tangrams.analysis;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.Stream;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineListener;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;
import javax.xml.bind.JAXBException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.audio.Sound;
import iristk.util.HAT;
import se.kth.speech.MutablePair;
import se.kth.speech.hat.xsd.Annotation;
import se.kth.speech.hat.xsd.Annotation.Segments.Segment;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources;
import se.kth.speech.hat.xsd.Annotation.Tracks.Track.Sources.Source;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see <a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.
 *
 */
public final class GameEventAudioReplayer implements Function<Segment, Stream<Entry<List<String>, DoubleStream>>> {

	private enum Parameter implements Supplier<Option> {
		HELP("?") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
			}
		},
		INPATH("i") {
			@Override
			public Option get() {
				return Option.builder(optName).longOpt("inpath").desc("The path to the HAT file corpus to process.")
						.hasArg().argName("path").type(File.class).required().build();
			}
		};

		protected final String optName;

		private Parameter(final String optName) {
			this.optName = optName;
		}

	}

//	private static class SourceSoundManager implements Function<File, Sound> {
//
//		private final Map<File, Sound> streamsByHref = new HashMap<>();
//		
//		private final File defaultParentFile;
//		
//		private SourceSoundManager(File defaultParentFile){
//			this.defaultParentFile = defaultParentFile;
//		}
//
//		/*
//		 * (non-Javadoc)
//		 *
//		 * @see java.util.function.Function#apply(java.lang.Object)
//		 */
//		@Override
//		public Sound apply(final File infile) {
//			return streamsByHref.computeIfAbsent(infile, k -> {
//				Sound result;
//				try {
//					result = new Sound(k);
//				} catch (IOException e) {
//					LOGGER.debug(String.format("An error occured while reading data at \"%s\"; Trying to resolve as a relative path.", infile), e); 
//					File resolvedInfile = new File(defaultParentFile, infile.toString());
//					try {
//						result = new Sound(resolvedInfile);
//					} catch (IOException fallbackE) {
//						throw new UncheckedIOException(fallbackE);
//					} catch (UnsupportedAudioFileException fallbackE) {
//						throw new IllegalArgumentException(fallbackE);
//					}
//				} catch (UnsupportedAudioFileException e) {
//					LOGGER.debug(String.format("An error occured while reading data at \"%s\"; Trying to resolve as a relative path.", infile), e); 
//					File resolvedInfile = new File(defaultParentFile, infile.toString());
//					try {
//						result = new Sound(resolvedInfile);
//					} catch (IOException fallbackE) {
//						throw new UncheckedIOException(fallbackE);
//					} catch (UnsupportedAudioFileException fallbackE) {
//						throw new IllegalArgumentException(fallbackE);
//					}
//				}
//				return result;
//			});
//		}
//
//	}
	
	
	private static class SourceSoundManager implements Function<String, Clip> {

		private final Map<String, Clip> streamsBySourceId = new HashMap<>();
		
		private final Function<? super String, String> sourceHrefFactory;
		
		private final File defaultParentFile;
		
		private SourceSoundManager(Function<? super String, String> sourceHrefFactory, File defaultParentFile){
			this.sourceHrefFactory = sourceHrefFactory;
			this.defaultParentFile = defaultParentFile;
		}
		
		
		private AudioInputStream createAudioInputStream(String href){
			AudioInputStream result;
			try {
				result = AudioSystem.getAudioInputStream(new File(href));
			} catch (UnsupportedAudioFileException | IOException e) {
				final File resolvedInfile = new File(defaultParentFile, href);
				LOGGER.debug(String.format("An error occurred while reading data at \"%s\"; Trying to resolve to path \"%s\".", href, resolvedInfile), e); 
				try {
					result = AudioSystem.getAudioInputStream(resolvedInfile);
				} catch (IOException fallbackE) {
					throw new UncheckedIOException(fallbackE);
				} catch (UnsupportedAudioFileException fallbackE) {
					throw new IllegalArgumentException(fallbackE);
				}
			}
			return result;
		}

		/*
		 * (non-Javadoc)
		 *
		 * @see java.util.function.Function#apply(java.lang.Object)
		 */
		@Override
		public Clip apply(final String sourceId) {
			return streamsBySourceId.computeIfAbsent(sourceId, k -> {
				String href = sourceHrefFactory.apply(sourceId);
				AudioInputStream is = createAudioInputStream(href);
				AudioFormat format = is.getFormat();
				DataLine.Info info = new DataLine.Info(Clip.class, format);
				Clip result;
				try {
					result = (Clip) AudioSystem.getLine(info);
					result.open();
				} catch (LineUnavailableException e) {
					throw new IllegalStateException(e);
				}
				return result;
			});
		}


		/* (non-Javadoc)
		 * @see java.lang.Object#finalize()
		 */
		@Override
		protected void finalize() throws Throwable {
			streamsBySourceId.values().forEach(stream -> {
				stream.close();
			});
			super.finalize();
		}
	}
	
	private static final int EXPECTED_UNIQUE_GAME_COUNT = 1;

	private static final Logger LOGGER = LoggerFactory.getLogger(GameEventAudioReplayer.class);

	/**
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4546093/1391325">StackOverflow</a>
	 */
	private static final Pattern MINIMAL_FILE_EXT_PATTERN = Pattern.compile("\\.(?=[^\\.]+$)");

	private static final Options OPTIONS = createOptions();

	private static final Function<Segment, List<Utterance>> SEG_UTT_FACTORY = new SegmentUtteranceFactory();

	private static final Collector<CharSequence, ?, String> TABLE_ROW_CELL_JOINER;

	private static final String TABLE_STRING_REPR_COL_DELIMITER;

	private static final String TABLE_STRING_REPR_ROW_DELIMITER = System.lineSeparator();

	static {
		TABLE_STRING_REPR_COL_DELIMITER = "\t";
		TABLE_ROW_CELL_JOINER = Collectors.joining(TABLE_STRING_REPR_COL_DELIMITER);
	}
	
	
	public static void main(final String[] args) throws IOException, JAXBException, InterruptedException {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cl = parser.parse(OPTIONS, args);
			if (cl.hasOption(Parameter.HELP.optName)) {
				printHelp();
			} else {
				final File inpath = (File) cl.getParsedOptionValue(Parameter.INPATH.optName);
				LOGGER.info("Reading annotations from \"{}\".", inpath);
				final Annotation uttAnnots = HAT.readAnnotation(inpath);
				
				final Map<String, String> sourceHrefs = uttAnnots.getTracks().getTrack().stream().map(Track::getSources)
						.map(Sources::getSource).flatMap(List::stream)
						.collect(Collectors.toMap(Source::getId, Source::getHref));
				final SourceSoundManager soundManager = new SourceSoundManager(sourceHrefs::get, inpath.getParentFile());
				final List<Segment> segments = uttAnnots.getSegments().getSegment();
				final Map<String, String> segmentSources = segments.stream()
						.collect(Collectors.toMap(Segment::getId, Segment::getSource));
				final List<Utterance> utts = segments.stream().map(SEG_UTT_FACTORY).flatMap(List::stream)
						.sorted(Comparator.comparing(Utterance::getStartTime)
								.thenComparing(Comparator.comparing(Utterance::getEndTime)))
						.collect(Collectors.toList());
				final Iterator<Utterance> uttIter = utts.iterator();
				while (uttIter.hasNext()) {
					final Utterance utt = uttIter.next();
					final String sourceId = segmentSources.get(utt.getSegmentId());
					LOGGER.info("Reading source audio for \"{}\".", sourceId);
					final Clip sourceSound = soundManager.apply(sourceId);
//					final long uttStartMicros = (long) Math.floor(utt.getStartTime() * SegmentTimes.TIME_TO_MILLS_FACTOR * 1000);
					final double uttStartMicros = utt.getStartTime() * SegmentTimes.TIME_TO_MILLS_FACTOR * 1000;
//					sourceSound.setMicrosecondPosition(uttStartMicros);
					
					final double uttEndMicros = utt.getEndTime() * SegmentTimes.TIME_TO_MILLS_FACTOR * 1000;
//					final long uttLengthMicros = (long) Math.ceil(uttEndMicros - uttStartMicros);
					System.out.println(String.format("Playing utterance audio: %s",
							utt.getTokens().stream().collect(Collectors.joining(" "))));
					
					AudioFormat audioFormat = sourceSound.getFormat();
					
					final double microsPerFrame = 1000000 / audioFormat.getFrameRate();
					LOGGER.info("microsPerFrame: " + microsPerFrame);
					final int uttStartFrame = (int) Math.floor(uttStartMicros / microsPerFrame);
					final int uttEndFrame = (int) Math.ceil(uttEndMicros / microsPerFrame); 
					sourceSound.setLoopPoints(uttStartFrame, uttEndFrame);
					sourceSound.loop(1);
//					sourceSound.start();
//					for (long micros = uttStartMicros; micros < uttEndMicros; micros += microsPerFrame ) {
//						 Do nothing
//					}
//					sourceSound.stop();
					
					
					
				}

				// final Map<String, String> sourceIdPlayerIds =
				// createSourceIdPlayerIdMap(uttAnnots);
				// final Set<String> playerIds = new
				// HashSet<>(sourceIdPlayerIds.values());
				// final int expectedEventLogFileCount = playerIds.size();
				// final Path sessionLogDir = inpath.getParentFile().toPath();
				// LOGGER.info("Processing session log directory \"{}\".",
				// sessionLogDir);
				// final Map<String, Path> playerEventLogFilePaths =
				// LoggedEvents
				// .createPlayerEventLogFileMap(sessionLogDir,
				// expectedEventLogFileCount);
				// final Table<String, String, GameHistory>
				// playerGameStateChangeData = LoggedEvents
				// .createPlayerGameHistoryTable(playerEventLogFilePaths.entrySet(),
				// EXPECTED_UNIQUE_GAME_COUNT);
				// final Set<String> playerGameIdIntersection = new
				// HashSet<>(playerGameStateChangeData.columnKeySet());
				// playerGameStateChangeData.rowMap().values().stream().map(Map::keySet)
				// .forEach(playerGameIdIntersection::retainAll);
				// final int gameCount = playerGameIdIntersection.size();
				// if (gameCount == 1) {
				// final Iterator<GameStateDescription> gameDescs =
				// playerGameStateChangeData.values().stream()
				// .map(GameHistory::getInitialState).iterator();
				// final GameStateDescription firstGameDesc = gameDescs.next();
				// while (gameDescs.hasNext()) {
				// // Sanity check to make sure that all players have
				// // started with the same game setup
				// final GameStateDescription next = gameDescs.next();
				// if (!firstGameDesc.isEquivalent(next)) {
				// throw new IllegalArgumentException("Found non-equivalent
				// initial states between players.");
				// }
				// }
				//
				// final int uniqueModelDescriptionCount =
				// playerGameStateChangeData.values().size();
				// final ToDoubleFunction<String> namedResourceEdgeCountFactory
				// = new ImageEdgeCountFactory();
				// final List<GameContextFeatureExtractor>
				// contextFeatureExtractors = Arrays
				// .asList(new
				// SelectedEntityFeatureExtractor(uniqueModelDescriptionCount,
				// namedResourceEdgeCountFactory));
				// final Stream.Builder<String> featureDescBuilder =
				// Stream.builder();
				// featureDescBuilder.accept("WORD");
				// contextFeatureExtractors.stream()
				// .map(extractor ->
				// extractor.createFeatureDescriptions(firstGameDesc))
				// .flatMap(Function.identity()).forEachOrdered(featureDescBuilder);
				//
				// final Stream<String> featureDescs =
				// featureDescBuilder.build();
				// final String header =
				// featureDescs.collect(TABLE_ROW_CELL_JOINER);
				//
				// final String gameId =
				// playerGameIdIntersection.iterator().next();
				// final Map<String, GameHistory> playerGameHistories =
				// playerGameStateChangeData.columnMap()
				// .get(gameId);
				// final UtteranceGameContextFactory uttContextFactory = new
				// UtteranceGameContextFactory(
				// playerGameHistories::get);
				// final GameEventAudioReplayer trainingDataFactory = new
				// GameEventAudioReplayer(
				// sourceIdPlayerIds, uttContextFactory,
				// contextFeatureExtractors);
				// final Stream<Stream<Entry<List<String>, DoubleStream>>>
				// segTrainingData = segments.stream()
				// .map(trainingDataFactory);
				// final Stream<Entry<List<String>, DoubleStream>> trainingData
				// = segTrainingData
				// .flatMap(Function.identity());
				//
				//
				//
				// } else {
				// throw new UnsupportedOperationException(
				// String.format("No logic for handling a game count of %d.",
				// gameCount));
				// }
			}
		} catch (final ParseException e) {
			System.out.println(String.format("An error occured while parsing the command-line arguments: %s", e));
			printHelp();
		}
	}

	private static Options createOptions() {
		final Options result = new Options();
		Arrays.stream(Parameter.values()).map(Parameter::get).forEach(result::addOption);
		return result;
	}

	private static Map<String, String> createSourceIdPlayerIdMap(final Annotation uttAnnots) {
		final List<Track> tracks = uttAnnots.getTracks().getTrack();
		final Stream<Source> sources = tracks.stream().map(Track::getSources).map(Sources::getSource)
				.flatMap(List::stream);
		return sources.collect(Collectors.toMap(Source::getId, source -> {
			final String href = source.getHref();
			return MINIMAL_FILE_EXT_PATTERN.split(href)[0];
		}));
	}

	private static void printHelp() {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(GameEventAudioReplayer.class.getName(), OPTIONS);
	}

	private final Map<String, String> sourceIdPlayerIds;

	private final BiFunction<? super Utterance, ? super String, Stream<Entry<Utterance, GameContext>>> uttContextFactory;

	/**
	 * @param contextFeatureExtractors
	 * @param playerGameHistories
	 * @param sourceIdPlayerIds
	 *
	 */
	public GameEventAudioReplayer(final Map<String, String> sourceIdPlayerIds,
			final BiFunction<? super Utterance, ? super String, Stream<Entry<Utterance, GameContext>>> uttContextFactory) {
		this.sourceIdPlayerIds = sourceIdPlayerIds;
		this.uttContextFactory = uttContextFactory;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Stream<Entry<List<String>, DoubleStream>> apply(final Segment segment) {
//		final List<Utterance> utts = SEG_UTT_FACTORY.apply(segment);
//		final String sourceId = segment.getSource();
//		// Get the player ID associated with the given audio source
//		final String playerId = sourceIdPlayerIds.get(sourceId);
//		final Stream<Entry<Utterance, GameContext>> uttContexts = utts.stream()
//				.flatMap(utt -> uttContextFactory.apply(utt, playerId));
//		return uttContexts.map(uttContext -> {
//			final DoubleStream.Builder featureVectorBuilder = DoubleStream.builder();
//			final DoubleStream vals = featureVectorBuilder.build();
//			final List<String> uttTokens = uttContext.getKey().getTokens();
//			return new MutablePair<>(uttTokens, vals);
//		});
		return null;
	}

}

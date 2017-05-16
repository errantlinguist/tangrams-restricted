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
package se.kth.speech.coin.tangrams.analysis.features;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.MutablePair;
import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.Utterance;
import weka.core.Instances;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since Apr 17, 2017
 * @see <a href="http://anthology.aclweb.org/W/W15/W15-0124.pdf">Casey
 *      Kennington, Livia Dia, &amp; David Schlangen. &ldquo;A Discriminative
 *      Model for Perceptually-Grounded Incremental Reference Resolution.&rdquo;
 *      In <em>Proceedings of IWCS 2015</em><a>.
 *
 */
public final class WordsAsClassifiersCrossValidationTestSetFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(WordsAsClassifiersCrossValidationTestSetFactory.class);

	@Inject
	private BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventDiagFactory;

	@Inject
	private Function<Collection<SessionEventDialogueManager>, Map<String, Instances>> instancesFactory;

	public Stream<Entry<SessionDataManager, Map<String, Instances>>> apply(
			final Map<SessionDataManager, Path> allSessions) throws IOException, JAXBException {
		final Stream.Builder<Entry<SessionDataManager, Map<String, Instances>>> resultBuilder = Stream.builder();
		for (final Entry<SessionDataManager, Path> testSessionDataEntry : allSessions.entrySet()) {
			final Path testSessionDataFilePath = testSessionDataEntry.getValue();
			LOGGER.info("Creating {}-fold cross-validation set for testing on session data from \"{}\".",
					allSessions.size(), testSessionDataFilePath);
			final SessionDataManager testSessionDataMgr = testSessionDataEntry.getKey();
			resultBuilder.accept(createTestSet(testSessionDataMgr, allSessions.keySet()));
		}
		return resultBuilder.build();
	}

	public Stream<Entry<SessionDataManager, Map<String, Instances>>> apply(final Set<SessionDataManager> allSessions)
			throws IOException, JAXBException {
		final Stream.Builder<Entry<SessionDataManager, Map<String, Instances>>> resultBuilder = Stream.builder();
		for (final SessionDataManager testSessionDataMgr : allSessions) {
			resultBuilder.accept(createTestSet(testSessionDataMgr, allSessions));
		}
		return resultBuilder.build();
	}

	private Entry<SessionDataManager, Map<String, Instances>> createTestSet(final SessionDataManager testSessionDataMgr,
			final Set<SessionDataManager> allSessions) throws JAXBException, IOException {
		final Map<String, Instances> classInstances;
		{
			final SessionDataManager[] trainingSessionDataMgrs = allSessions.stream()
					.filter(sessionData -> !sessionData.equals(testSessionDataMgr)).toArray(SessionDataManager[]::new);
			final List<SessionEventDialogueManager> trainingSessionEvtDiagMgrs = new ArrayList<>(
					trainingSessionDataMgrs.length);
			for (final SessionDataManager trainingSessionDatum : trainingSessionDataMgrs) {
				final SessionEventDialogueManager sessionEventDiagMgr = new SessionEventDialogueManager(
						trainingSessionDatum, eventDiagFactory);
				trainingSessionEvtDiagMgrs.add(sessionEventDiagMgr);
			}
			classInstances = instancesFactory.apply(trainingSessionEvtDiagMgrs);
		}
		LOGGER.info("Read training data for {} class(es).", classInstances.size());
		return new MutablePair<>(testSessionDataMgr, classInstances);
	}

}

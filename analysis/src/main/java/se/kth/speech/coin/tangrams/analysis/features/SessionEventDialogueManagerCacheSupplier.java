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
import java.util.ListIterator;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.stream.Stream;

import javax.inject.Inject;
import javax.xml.bind.JAXBException;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import se.kth.speech.coin.tangrams.analysis.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.analysis.SessionDataManager;
import se.kth.speech.coin.tangrams.analysis.SessionEventDialogueManager;
import se.kth.speech.coin.tangrams.analysis.Utterance;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 16, 2017
 *
 */
public final class SessionEventDialogueManagerCacheSupplier
		implements Supplier<LoadingCache<SessionDataManager, SessionEventDialogueManager>> {

	@Inject
	private BiFunction<ListIterator<Utterance>, GameHistory, Stream<EventDialogue>> eventDiagFactory;

	private final LoadingCache<SessionDataManager, SessionEventDialogueManager> instances = CacheBuilder.newBuilder()
			.build(new CacheLoader<SessionDataManager, SessionEventDialogueManager>() {

				@Override
				public SessionEventDialogueManager load(final SessionDataManager key)
						throws JAXBException, IOException {
					return new SessionEventDialogueManager(key, eventDiagFactory);
				}

			});

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public LoadingCache<SessionDataManager, SessionEventDialogueManager> get() {
		return instances;
	}

}

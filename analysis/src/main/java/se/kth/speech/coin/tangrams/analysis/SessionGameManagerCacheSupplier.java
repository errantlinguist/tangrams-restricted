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

import java.io.IOException;
import java.util.function.Supplier;

import javax.inject.Named;
import javax.xml.bind.JAXBException;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import se.kth.speech.coin.tangrams.analysis.io.SessionDataManager;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since May 16, 2017
 *
 */
@Named
public final class SessionGameManagerCacheSupplier
		implements Supplier<LoadingCache<SessionDataManager, SessionGameManager>> {
	
	private static final int ESTIMATED_UNIQUE_SESSION_COUNT = 50;

	private final LoadingCache<SessionDataManager, SessionGameManager> instances = CacheBuilder.newBuilder()
			.softValues().initialCapacity(ESTIMATED_UNIQUE_SESSION_COUNT).maximumSize(ESTIMATED_UNIQUE_SESSION_COUNT)
			.build(new CacheLoader<SessionDataManager, SessionGameManager>() {

				@Override
				public SessionGameManager load(final SessionDataManager key) throws IOException, JAXBException {
					return new SessionGameManager(key);
				}

			});

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Supplier#get()
	 */
	@Override
	public LoadingCache<SessionDataManager, SessionGameManager> get() {
		return instances;
	}

}

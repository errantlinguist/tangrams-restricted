/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import com.google.common.collect.Maps;

import se.kth.speech.hat.xsd.Annotation.Segments.Segment;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 4 May 2017
 *
 */
public final class UtterancePlayerIdMapFactory implements Function<Collection<Segment>, Map<Utterance, String>> {

	private final Function<Segment, List<Utterance>> segUttFactory;

	private final Function<? super String, String> sourcePlayerIdGetter;

	public UtterancePlayerIdMapFactory(final Function<Segment, List<Utterance>> segUttFactory,
			final Function<? super String, String> sourcePlayerIdGetter) {
		this.segUttFactory = segUttFactory;
		this.sourcePlayerIdGetter = sourcePlayerIdGetter;
	}

	@Override
	public Map<Utterance, String> apply(final Collection<Segment> segments) {
		final Map<Utterance, String> result = Maps.newHashMapWithExpectedSize(segments.size());
		for (final Segment segment : segments) {
			final String sourceId = segment.getSource();
			final List<Utterance> segUtts = segUttFactory.apply(segment);
			for (final Utterance segUtt : segUtts) {
				final String playerId = sourcePlayerIdGetter.apply(sourceId);
				result.put(segUtt, playerId);
			}
		}
		return result;
	}

}

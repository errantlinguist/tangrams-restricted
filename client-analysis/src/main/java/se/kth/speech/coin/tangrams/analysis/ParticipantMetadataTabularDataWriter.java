/*
 *  This file is part of Tangrams-restricted.
 *
 *  Tangrams-restricted is free software: you can redistribute it and/or modify
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

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ArrayTable;
import com.google.common.collect.BiMap;
import com.google.common.collect.Maps;
import com.google.common.collect.RowSortedTable;
import com.google.common.collect.Table;
import com.google.common.collect.TreeBasedTable;

import se.kth.speech.ObservationOrderComparator;
import se.kth.speech.coin.tangrams.game.PlayerRole;

/**
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 23 Aug 2017
 *
 */
final class ParticipantMetadataTabularDataWriter { // NO_UCD (unused code)

	private static class ParticipantMetadatumNameComparator implements Comparator<String> {

		private static ParticipantMetadatum parseNullableMetadatum(final String name) {
			ParticipantMetadatum result = null;
			try {
				result = ParticipantMetadatum.valueOf(name);
			} catch (final IllegalArgumentException e) {
				LOGGER.debug(String.format("Unable to parse \"%s\" as an instance of %s; Returning null.", name,
						ParticipantMetadatum.class), e);
			}
			return result;
		}

		private final Comparator<String> headerRowNameComparator;

		private final Comparator<String> rowObservationOrderComparator;

		private ParticipantMetadatumNameComparator(final String headerRowName, final int expectedRowCount) {
			headerRowNameComparator = Comparator.comparing(name -> !headerRowName.equals(name));
			rowObservationOrderComparator = new ObservationOrderComparator<>(expectedRowCount);
		}

		@Override
		public int compare(final String o1, final String o2) {
			int result = headerRowNameComparator.compare(o1, o2);
			if (result == 0) {
				final ParticipantMetadatum m1 = parseNullableMetadatum(o1);
				final ParticipantMetadatum m2 = parseNullableMetadatum(o2);
				if (m1 == null) {
					if (m2 == null) {
						result = rowObservationOrderComparator.compare(o1, o2);
					} else {
						result = 1;
					}
				} else if (m2 == null) {
					result = -1;
				} else {
					result = m1.compareTo(m2);
				}

			}

			return result;
		}

	}

	private static final int ESTIMATED_PARTICIPANT_METADATUM_COUNT = 21;

	private static final Logger LOGGER = LoggerFactory.getLogger(ParticipantMetadataTabularDataWriter.class);

	private static final Comparator<String> PARTICIPANT_ID_ORDERING_COMPARATOR = Comparator.naturalOrder();

	private static final String PARTICIPANT_METADATA_HEADER_ROW_NAME = "PARTICIPANT_ID";

	static Table<ParticipantMetadatum, String, String> createParticipantMetadataReprTable(
			final SessionGame canonicalGame, final BiMap<String, String> playerSourceIds,
			final BiMap<String, String> participantSourceIds) {
		final List<String> sortedParticipantIds = new ArrayList<>(participantSourceIds.keySet());
		sortedParticipantIds.sort(PARTICIPANT_ID_ORDERING_COMPARATOR);
		final Table<ParticipantMetadatum, String, String> result = ArrayTable
				.create(Arrays.asList(ParticipantMetadatum.values()), sortedParticipantIds);

		{
			final BiMap<String, PlayerRole> playerRoles = canonicalGame.getHistory().getInitialState().getPlayerRoles()
					.inverse();
			final BiMap<String, String> sourceParticipantIds = participantSourceIds.inverse();
			playerRoles.forEach((playerId, role) -> {
				final String sourceId = playerSourceIds.get(playerId);
				final String participantId = sourceParticipantIds.get(sourceId);
				final String oldRole = result.put(ParticipantMetadatum.INITIAL_ROLE, participantId, role.toString());
				assert oldRole == null;
			});
		}

		for (final Entry<String, String> participantSourceId : participantSourceIds.entrySet()) {
			final String oldSourceId = result.put(ParticipantMetadatum.SOURCE_ID, participantSourceId.getKey(), participantSourceId.getValue());
			assert oldSourceId == null;
		}
		assert result.rowKeySet().size() == ParticipantMetadatum.values().length;
		assert result.columnKeySet().size() == sortedParticipantIds.size();
		return result;
	}

	private final Charset outputCharset;

	private final Collector<CharSequence, ?, String> tableRowCellJoiner;

	private final Pattern tableStrReprColDelimPattern;

	ParticipantMetadataTabularDataWriter(final String tableStrReprColDelim, final Charset outputCharset) {
		tableStrReprColDelimPattern = Pattern.compile(tableStrReprColDelim);
		tableRowCellJoiner = Collectors.joining(tableStrReprColDelim);
		this.outputCharset = outputCharset;
	}

	private LinkedHashMap<String, List<String>> readHeadedRowMap(final Path infilePath,
			final int expectedMetadatumCount) throws IOException {
		final LinkedHashMap<String, List<String>> result = Maps
				.newLinkedHashMapWithExpectedSize(expectedMetadatumCount);
		try (BufferedReader metadataRowReader = Files.newBufferedReader(infilePath, outputCharset)) {
			for (String row = metadataRowReader.readLine(); row != null; row = metadataRowReader.readLine()) {
				final List<String> rowCells = Arrays.asList(tableStrReprColDelimPattern.split(row));
				final int currentRowLength = rowCells.size();
				if (currentRowLength < 2) {
					LOGGER.warn("Row has only {} value(s): {}", currentRowLength, rowCells);
				}
				final String metadatumCell = rowCells.get(0);
				final List<String> values = rowCells.subList(1, currentRowLength);
				final List<String> extantValues = result.put(metadatumCell, values);
				if (extantValues != null) {
					throw new IllegalArgumentException(
							String.format("More than one row found for metadatum \"%s\".", metadatumCell));
				}

			}
		} catch (final NoSuchFileException e) {
			LOGGER.debug("No already-persisted metadata found at \"{}\".", infilePath);
		}
		return result;
	}

	private RowSortedTable<String, String, String> readParticipantMetadata(final Path infilePath) throws IOException {
		final LinkedHashMap<String, List<String>> metadatumRows = readHeadedRowMap(infilePath,
				ESTIMATED_PARTICIPANT_METADATUM_COUNT);
		final List<String> extantParticipantIds = metadatumRows.getOrDefault(PARTICIPANT_METADATA_HEADER_ROW_NAME,
				Collections.emptyList());
		final RowSortedTable<String, String, String> result = TreeBasedTable
				.create(new ParticipantMetadatumNameComparator(PARTICIPANT_METADATA_HEADER_ROW_NAME,
						ESTIMATED_PARTICIPANT_METADATUM_COUNT), PARTICIPANT_ID_ORDERING_COMPARATOR);
		final Stream<Entry<String, List<String>>> nonHeaderRows = metadatumRows.entrySet().stream()
				.filter(metadatumRow -> !metadatumRow.getKey().equals(PARTICIPANT_METADATA_HEADER_ROW_NAME));
		nonHeaderRows.forEach(metadatumRow -> {
			final String metadatumName = metadatumRow.getKey();
			final int participantCount = extantParticipantIds.size();
			final List<String> paddedParticipantValues = new ArrayList<>(participantCount);
			paddedParticipantValues.addAll(metadatumRow.getValue());
			assert paddedParticipantValues.size() <= participantCount;
			while (paddedParticipantValues.size() < participantCount) {
				paddedParticipantValues.add("");
			}

			final Iterator<String> extantParticipantIdIter = extantParticipantIds.iterator();
			final Iterator<String> participantValueIter = paddedParticipantValues.iterator();
			while (extantParticipantIdIter.hasNext()) {
				final String extantParticipantId = extantParticipantIdIter.next();
				final String participantValue = participantValueIter.next();
				final String oldParticipantValue = result.put(metadatumName, extantParticipantId, participantValue);
				assert oldParticipantValue == null;
			}
		});
		return result;
	}

	void persistParticipantMetadata(final Table<ParticipantMetadatum, String, String> metadataParticipantValues,
			final Path outfilePath) throws IOException {
		// NOTE: This is not atomic: The OS could write to the file between its
		// reading and rewriting
		final RowSortedTable<String, String, String> unifiedMetadata = readParticipantMetadata(outfilePath);
		for (final Entry<ParticipantMetadatum, Map<String, String>> metadatumParticipantValues : metadataParticipantValues
				.rowMap().entrySet()) {
			final String metadatumName = metadatumParticipantValues.getKey().toString();
			for (final Entry<String, String> participantValue : metadatumParticipantValues.getValue().entrySet()) {
				final String oldParticipantValue = unifiedMetadata.put(metadatumName, participantValue.getKey(), participantValue.getValue());
				assert oldParticipantValue == null;
			}
		}

		// Pre-fetch all row-value map entries in order to avoid a
		// ConcurrentModificationException being thrown
		final List<Entry<String, Map<String, String>>> participantRowValueMaps = new ArrayList<>(
				unifiedMetadata.columnMap().entrySet());
		for (final Entry<String, Map<String, String>> participantRowValues : participantRowValueMaps) {
			final String participantId = participantRowValues.getKey();
			final Map<String, String> rowValues = participantRowValues.getValue();
			// Put the participant ID as a cell value into the table so that it
			// gets printed on its own row
			rowValues.put(PARTICIPANT_METADATA_HEADER_ROW_NAME, participantId);
		}

		{
			final Stream<List<String>> metadataRows = unifiedMetadata.rowMap().entrySet().stream().map(entry -> {
				final String metadatumName = entry.getKey();
				final Map<String, String> participantValues = entry.getValue();
				final List<String> metadataRow = new ArrayList<>(participantValues.size() + 1);
				metadataRow.add(metadatumName);
				final Stream<Entry<String, String>> sortedParticipantValues = participantValues.entrySet().stream()
						.sorted(Comparator.comparing(Entry::getKey, PARTICIPANT_ID_ORDERING_COMPARATOR));
				sortedParticipantValues.forEach(participantValue -> {
					final String value = participantValue.getValue();
					metadataRow.add(value);
				});
				return metadataRow;
			});
			final Stream<String> metadataFileRows = metadataRows.map(List::stream)
					.map(stream -> stream.collect(tableRowCellJoiner));
			Files.write(outfilePath, (Iterable<String>) metadataFileRows::iterator, outputCharset,
					StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
		}
	}

}

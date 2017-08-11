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
package se.kth.speech.coin.tangrams.analysis.view;

import java.util.EnumMap;
import java.util.Map;

import javax.swing.JTable;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 11 Aug 2017
 *
 */
final class EventDialogueAttributeTables {

	private static final Map<EventDialogueAttribute, Integer> EVENT_DIALOGUE_ATTR_IDXS = createEventDialogueAttributeIndexMap();

	private static final Logger LOGGER = LoggerFactory.getLogger(EventDialogueAttributeTables.class);

	private static Map<EventDialogueAttribute, Integer> createEventDialogueAttributeIndexMap() {
		final Map<EventDialogueAttribute, Integer> result = new EnumMap<>(EventDialogueAttribute.class);
		final EventDialogueAttribute[] attrs = EventDialogueAttribute.values();
		for (int i = 0; i < attrs.length; ++i) {
			final EventDialogueAttribute attr = attrs[i];
			result.put(attr, i);
		}
		assert result.size() == attrs.length;
		return result;
	}

	private static TableColumn getEventDialogueAttributeColumn(final TableColumnModel colModel,
			final EventDialogueAttribute attr) {
		final int colIdx = EVENT_DIALOGUE_ATTR_IDXS.get(attr);
		return colModel.getColumn(colIdx);
	}

	static Map<EventDialogueAttribute, Integer> createEventDialogueAttributeColumnIndexMap(final JTable table) {
		final Map<EventDialogueAttribute, Integer> result = new EnumMap<>(EventDialogueAttribute.class);
		final EventDialogueAttribute[] attrs = EventDialogueAttribute.values();
		for (int colIdx = 0; colIdx < table.getColumnCount(); ++colIdx) {
			final String colName = table.getColumnName(colIdx);
			try {
				final EventDialogueAttribute evtDiagAttr = EventDialogueAttribute.valueOf(colName);
				final Integer oldValue = result.put(evtDiagAttr, colIdx);
				assert oldValue == null;
			} catch (final IllegalArgumentException e) {
				LOGGER.debug("Column index {} with name \"{}\" does not represent a(n) {}; Ignoring.", colIdx, colName,
						EventDialogueAttribute.class.getSimpleName());
			}
		}
		assert result.size() == attrs.length;
		return result;
	}

	static Map<EventDialogueAttribute, TableColumn> createEventDialogueAttributeColumnMap(
			final TableColumnModel colModel) {
		final Map<EventDialogueAttribute, TableColumn> result = new EnumMap<>(EventDialogueAttribute.class);
		final EventDialogueAttribute[] attrs = EventDialogueAttribute.values();
		for (final EventDialogueAttribute attr : EventDialogueAttribute.values()) {
			result.put(attr, getEventDialogueAttributeColumn(colModel, attr));
		}
		assert result.size() == attrs.length;
		return result;
	}

	private EventDialogueAttributeTables() {
	}

}

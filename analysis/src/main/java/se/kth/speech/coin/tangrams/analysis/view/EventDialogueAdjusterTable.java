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
package se.kth.speech.coin.tangrams.analysis.view;

import java.awt.Component;
import java.util.stream.IntStream;

import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.analysis.dialogues.EventDialogue;
import se.kth.speech.coin.tangrams.analysis.dialogues.Utterance;

final class EventDialogueAdjusterTable extends JTable {

	private static final String NULL_VALUE_REPR = "-";

	/**
	 *
	 */
	private static final long serialVersionUID = 5176564593731003375L;

	private static final Logger LOGGER = LoggerFactory.getLogger(EventDialogueAdjusterTable.class);

	private static TableCellRenderer getColumnHeaderRenderer(final TableColumn tableColumn, final JTableHeader header) {
		final TableCellRenderer colRenderer = tableColumn.getHeaderRenderer();
		return colRenderer == null ? header.getDefaultRenderer() : colRenderer;
	}

	private static void setColumnEventDialogueAttributeRenderers(final TableColumnModel colModel) {
		final EventDialogueAttribute[] attrs = EventDialogueAttribute.values();
		for (int i = 0; i < attrs.length; ++i) {
			final EventDialogueAttribute attr = attrs[i];
			final TableColumn col = colModel.getColumn(i);
			switch (attr) {
			case FIRST_EVENT_SENDER: {
				col.setCellRenderer(
						new EventDialogueEventSenderRenderer(EventDialogue::getFirstEvent, NULL_VALUE_REPR));
				break;
			}
			case FIRST_EVENT_TIME: {
				col.setCellRenderer(new EventDialogueEventTimeRenderer(EventDialogue::getFirstEvent, NULL_VALUE_REPR));
				break;
			}
			case LAST_EVENT_TIME: {
				col.setCellRenderer(new EventDialogueEventTimeRenderer(EventDialogue::getLastEvent, NULL_VALUE_REPR));
				break;
			}
			default: {
				throw new AssertionError("No logic for handing switch case.");
			}
			}
		}
	}

	EventDialogueAdjusterTable(final TableModel dm, final UtteranceCellRenderer uttCellRenderer) {
		super(dm);
		setCellSelectionEnabled(true);
		setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

		setColumnEventDialogueAttributeRenderers(getColumnModel());
		setDefaultRenderer(Utterance.class, uttCellRenderer);

		// Set widths using the newly-set renderers
		IntStream.range(0, getColumnCount()).forEach(this::setOptimumPreferredWidth);
	}

	private void setOptimumPreferredWidth(final int columnIdx) {
		final JTableHeader header = getTableHeader();
		final TableColumnModel colModel = getColumnModel();
		final TableColumn tableColumn = colModel.getColumn(columnIdx);
		final TableCellRenderer colRenderer = getColumnHeaderRenderer(tableColumn, header);
		final Object colHeaderValue = tableColumn.getHeaderValue();
		LOGGER.debug("Calculating width for column header value \"{}\", for column {}.", colHeaderValue, columnIdx);
		final Component colHeaderCellRendererComponent = colRenderer.getTableCellRendererComponent(this, colHeaderValue,
				false, false, -1, columnIdx);
		final int headerCellWdith = colHeaderCellRendererComponent.getPreferredSize().width;

		// https://tips4java.wordpress.com/2008/11/10/table-column-adjuster/
		final int intercellSpacingWidth = getIntercellSpacing().width;
		final int maxWidth = tableColumn.getMaxWidth();
		int preferredWidth = Math.max(headerCellWdith, tableColumn.getMinWidth());
		if (preferredWidth < maxWidth) {
			for (int row = 0; row < getRowCount(); row++) {
				final TableCellRenderer cellRenderer = getCellRenderer(row, columnIdx);
				final Component c = prepareRenderer(cellRenderer, row, columnIdx);
				final int cellWidth = c.getPreferredSize().width + intercellSpacingWidth;
				preferredWidth = Math.max(preferredWidth, cellWidth);

				if (preferredWidth >= maxWidth) {
					// We've exceeded the maximum width, no need to check
					// other rows
					preferredWidth = maxWidth;
					break;
				}
			}
		} else {
			// We've exceeded the maximum width, no need to check the rows
			preferredWidth = maxWidth;
		}
		tableColumn.setPreferredWidth(preferredWidth);
	}

}
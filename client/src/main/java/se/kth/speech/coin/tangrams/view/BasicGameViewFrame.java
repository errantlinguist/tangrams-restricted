/*
 *  This file is part of client.
 *
 *  client is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator.Attribute;
import java.util.Collections;
import java.util.Map;
import java.util.function.Supplier;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import com.google.common.collect.Maps;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.game.Controller;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
public class BasicGameViewFrame extends JFrame {

	/**
	 *
	 */
	private static final long serialVersionUID = -4129777933223228599L;

	private static Map<Attribute, Object> createInfoFontAttrMap() {
		final Map<Attribute, Object> result = Maps.newHashMapWithExpectedSize(2);
		result.put(TextAttribute.FAMILY, Font.SANS_SERIF);
		result.put(TextAttribute.SIZE, 20.0f);
		return result;
	}

	BasicGameViewFrame(final AbstractGameBoardPanel boardPanel, final Controller controller,
			final Supplier<? extends MapEntryRemapping<Integer, SpatialRegion>> moveFactory,
			final Dimension preferredSize) {
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		setPreferredSize(preferredSize);
		setLayout(new BorderLayout());
		add(boardPanel, BorderLayout.CENTER);

		{
			final JPanel statusPanel = new JPanel();
			add(statusPanel, BorderLayout.PAGE_END);
			statusPanel.setLayout(new BoxLayout(statusPanel, BoxLayout.LINE_AXIS));

			{
				final JTable controllerInfoTable = new JTable(new ControllerInfoTableModel(controller));
				final Font infoFont = controllerInfoTable.getFont().deriveFont(createInfoFontAttrMap());
				controllerInfoTable.setFont(infoFont);
				controllerInfoTable.setRowSelectionAllowed(false);
				controllerInfoTable.setColumnSelectionAllowed(false);
				controllerInfoTable.setCellSelectionEnabled(false);
				final JPanel tablePanel = new JPanel();
				statusPanel.add(tablePanel);
				tablePanel.setLayout(new BoxLayout(tablePanel, BoxLayout.PAGE_AXIS));
				final JTableHeader tableHeader = controllerInfoTable.getTableHeader();
				tableHeader.setFont(
						infoFont.deriveFont(Collections.singletonMap(TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD)));
				tablePanel.add(tableHeader);
				tablePanel.add(controllerInfoTable);
			}
		}
	}
}

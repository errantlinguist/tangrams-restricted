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
import java.util.NoSuchElementException;
import java.util.function.Supplier;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.Turn;
import se.kth.speech.coin.tangrams.iristk.events.Move;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
public class BasicGameViewFrame extends JFrame implements Controller.Listener {

	private static final Logger LOGGER = LoggerFactory.getLogger(BasicGameViewFrame.class);

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

	private final Runnable nextTurnHook;

	BasicGameViewFrame(final AbstractGameBoardPanel boardPanel, final Controller controller,
			final Supplier<? extends MapEntryRemapping<Integer, SpatialRegion>> moveFactory,
			final Dimension preferredSize) {
		controller.getListeners().add(this);
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

		nextTurnHook = () -> {
			try {
				final MapEntryRemapping<Integer, SpatialRegion> nextMove = moveFactory.get();
				final Integer pieceId = nextMove.getKey();
				final SpatialRegion source = nextMove.getOldValue();
				final SpatialRegion target = nextMove.getNewValue();
				boardPanel.notifyNextMove(source, target, pieceId);
			} catch (final NoSuchElementException e) {
				// No pieces left to be moved; Game cannot continue
				JOptionPane.showMessageDialog(this, "No more moves available.", "No more moves",
						JOptionPane.WARNING_MESSAGE);
				LOGGER.warn("No more moves available.", e);
			}
		};
	}

	@Override
	public void updateNextMove(final Move move) {
		LOGGER.debug("Observed event representing the subbmission of a move by a player.");
	}

	@Override
	public void updatePlayerJoined(final String joinedPlayerId, final long time) {
		LOGGER.debug("Observed event representing the joining of a player.");
	}

	@Override
	public void updatePlayerRole(final PlayerRole newRole) {
		LOGGER.debug("Observed event representing a change in player role.");
		updateNextTurnResponsibility(newRole);
	}

	@Override
	public void updatePlayerSelection(final Integer pieceId, final SpatialRegion region) {
		LOGGER.debug("Observed event representing a user selection.");
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateScore(int)
	 */
	@Override
	public void updateScore(final int score) {
		LOGGER.debug("Notified of new score.");
	}

	@Override
	public void updateSelectionRejected(final Integer pieceId, final SpatialRegion region) {
		LOGGER.debug("Observed event representing the rejection of the last selection.");
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see se.kth.speech.coin.tangrams.iristk.Controller.Listener#
	 * updateTurnCompletion(se.kth.speech.coin.tangrams.iristk.events.Turn)
	 */
	@Override
	public void updateTurnCompleted(final Turn turn) {
		LOGGER.debug("Observed event representing a completed turn.");
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateMoveCount(int)
	 */
	@Override
	public void updateTurnCount(final int newCount) {
		LOGGER.debug("Notified of new turn count.");
	}

	private void updateNextTurnResponsibility(final PlayerRole role) {
		if (PlayerRole.MOVE_SUBMISSION.equals(role)) {
			JOptionPane.showMessageDialog(this, "Press \"OK\" to continue to the next turn.", "Next turn",
					JOptionPane.INFORMATION_MESSAGE);
			nextTurnHook.run();
		}
	}

}

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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator.Attribute;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.MapEntryRemapping;
import se.kth.speech.MutablePair;
import se.kth.speech.SpatialRegion;
import se.kth.speech.awt.CachingMaximumWidthFontFactory;
import se.kth.speech.awt.ComponentResizedEventListener;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.Turn;
import se.kth.speech.coin.tangrams.iristk.events.Move;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameViewFrame extends JFrame implements Controller.Listener {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameViewFrame.class);

	private static final int MIN_ROLE_STATUS_LABEL_PADDING = 10;

	private static final Map<PlayerRole, String> PLAYER_ROLE_STATUS_LABEL_TEXT = createPlayerRoleStatusLabelTextMap();

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

	private static Map<PlayerRole, String> createPlayerRoleStatusLabelTextMap() {
		final Map<PlayerRole, String> result = new EnumMap<>(PlayerRole.class);
		result.put(PlayerRole.SELECTING, "Select piece to move.");
		result.put(PlayerRole.MOVE_SUBMISSION, "Continue to the next turn.");
		result.put(PlayerRole.SELECTION_CONFIRMATION, "Checking the other player's selection...");
		result.put(PlayerRole.WAITING_FOR_NEXT_MOVE, "Waiting for other player to submit next turn...");
		result.put(PlayerRole.WAITING_FOR_SELECTION, "Waiting for other player to select a piece to move...");
		result.put(PlayerRole.WAITING_FOR_SELECTION_CONFIRMATION, "Waiting for other player to confirm selection...");
		assert result.size() == PlayerRole.values().length;
		return result;
	}

	private static Map<Attribute, Object> createRoleStatusFontAttrMap() {
		final Map<Attribute, Object> result = Maps.newHashMapWithExpectedSize(2);
		result.put(TextAttribute.FAMILY, Font.SANS_SERIF);
		result.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD);
		// result.put(TextAttribute.SIZE, 32.0f);
		return result;
	}

	private static IntConsumer createRoleStatusLabelFontSizeUpdater(final Component comp,
			final float searchSizeIncrement, final Function<? super Font, FontMetrics> fmFactory) {
		final Font initialFont = comp.getFont();
		// Initialize the list to a size which is a function of the size of
		// the font as an estimate of the amount of fonts to search through
		final double maxSmallerFontCount = Math.ceil(comp.getFont().getSize2D());
		final double estBiggerFontCount = Math.ceil(maxSmallerFontCount / Math.log(maxSmallerFontCount));
		final int initialCapacity = (int) (maxSmallerFontCount + estBiggerFontCount);
		final List<Entry<Font, FontMetrics>> incrementingSizeFonts = new ArrayList<>(initialCapacity);
		final float startSize = 1.0f;
		{
			// Pre-populate expected possible re-sized fonts
			float size = startSize;
			while (incrementingSizeFonts.size() < initialCapacity) {
				final Font font = initialFont.deriveFont(size);
				final FontMetrics fm = fmFactory.apply(font);
				incrementingSizeFonts.add(new MutablePair<>(font, fm));
				size += searchSizeIncrement;
			}
		}
		return newWidth -> {
			final float endSize = Float.MAX_VALUE;
			final int padding = Math.max(newWidth / 24, MIN_ROLE_STATUS_LABEL_PADDING);
			final CachingMaximumWidthFontFactory fontFactory = new CachingMaximumWidthFontFactory(newWidth, fmFactory,
					initialFont, startSize, endSize, searchSizeIncrement, padding, incrementingSizeFonts::listIterator);
			final Font smallestRoleStatusLabelFont = PLAYER_ROLE_STATUS_LABEL_TEXT.values().stream().map(fontFactory)
					.collect(Collectors.minBy(Comparator.comparing(Font::getSize2D))).get();
			comp.setFont(smallestRoleStatusLabelFont);
		};
	}

	private final Runnable nextTurnHook;

	private final JLabel roleStatusLabel;

	private final IntConsumer roleStatusLabelFontSizeUpdater;

	GameViewFrame(final InteractiveGameBoardPanel boardPanel, final Controller controller,
			final Supplier<? extends MapEntryRemapping<Integer, SpatialRegion>> moveFactory,
			final Dimension preferredSize) {
		controller.getListeners().add(this);
		setPreferredSize(preferredSize);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		setLayout(new BorderLayout());
		add(boardPanel, BorderLayout.CENTER);

		final PlayerRole initialRole = controller.getRole();
		{
			final String labelText = PLAYER_ROLE_STATUS_LABEL_TEXT.get(initialRole);
			roleStatusLabel = new JLabel(labelText);
			final Font initialFont = roleStatusLabel.getFont().deriveFont(createRoleStatusFontAttrMap());
			roleStatusLabel.setFont(initialFont);
			roleStatusLabelFontSizeUpdater = createRoleStatusLabelFontSizeUpdater(roleStatusLabel, 1.0f,
					this::getFontMetrics);
			roleStatusLabelFontSizeUpdater.accept(preferredSize.width);
			addComponentListener(new ComponentResizedEventListener(this::updateRoleStatusLabelFontSize));
		}
		{
			final JPanel roleStatusPanel = new JPanel();
			roleStatusPanel.add(roleStatusLabel);
			add(roleStatusPanel, BorderLayout.PAGE_START);
		}

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
		addComponentListener(new ComponentAdapter() {

			/*
			 * (non-Javadoc)
			 *
			 * @see
			 * java.awt.event.ComponentAdapter#componentShown(java.awt.event
			 * .ComponentEvent)
			 */
			@Override
			public void componentShown(final ComponentEvent e) {
				updateNextTurnResponsibility(initialRole);
				removeComponentListener(this);
			}

		});
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
		final String roleStatusText = PLAYER_ROLE_STATUS_LABEL_TEXT.get(newRole);
		LOGGER.info("Setting player state label for role {}.", newRole);
		roleStatusLabel.setText(roleStatusText);
		final String labelText = roleStatusLabel.getText();
		assert labelText != null && !labelText.isEmpty();
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

	private void updateRoleStatusLabelFontSize() {
		roleStatusLabelFontSizeUpdater.accept(getWidth());
	}

}

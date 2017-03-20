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
import java.awt.Window;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator.Attribute;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.Random;
import java.util.Set;
import java.util.function.Supplier;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Maps;

import se.kth.speech.awt.ColorIcon;
import se.kth.speech.coin.tangrams.game.LocalController;
import se.kth.speech.coin.tangrams.game.PlayerJoinTime;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.RemoteController;
import se.kth.speech.coin.tangrams.iristk.events.ActivePlayerChange;
import se.kth.speech.coin.tangrams.iristk.events.GameEnding;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 2 Mar 2017
 *
 */
final class GameViewFrame extends JFrame implements Observer {

	private static final Logger LOGGER = LoggerFactory.getLogger(GameViewFrame.class);

	/**
	 *
	 */
	private static final long serialVersionUID = -4129777933223228599L;

	private static Map<Attribute, Object> createMoveCounterFontAttrMap() {
		final Map<Attribute, Object> result = Maps.newHashMapWithExpectedSize(3);
		result.put(TextAttribute.FAMILY, Font.SANS_SERIF);
		result.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD);
		result.put(TextAttribute.SIZE, 20.0f);
		return result;
	}

	private static MoveCounterLabel createMoveCounterLabel(final int initialMoveCount, final int siblingWidth) {
		final MoveCounterLabel result = new MoveCounterLabel(initialMoveCount);
		final Font font = Font.getFont(createMoveCounterFontAttrMap());
		result.setFont(font);
		{
			final FontMetrics fontMetrics = result.getFontMetrics(font);
			final int maxFontWidth = Arrays.stream(fontMetrics.getWidths()).max().getAsInt();
			final int preferredWidth = Math.max(siblingWidth, maxFontWidth * 3);
			final int preferredHeight = fontMetrics.getHeight();
			result.setPreferredSize(new Dimension(preferredWidth, preferredHeight));
		}
		return result;
	}

	private static ReadinessIndicator createPlayerReadinessIndicator(final int gameBoardPanelWidth,
			final PlayerTurnStatus initialStatus) {
		final ReadinessIndicator result = new ReadinessIndicator(
				new ColorIcon(gameBoardPanelWidth / 20, ReadinessIndicator.getStatusColor(initialStatus)),
				initialStatus);
		result.setAlignmentX(CENTER_ALIGNMENT);
		final String desc = "An indicator showing if it's your turn or not: Green means \"ready\" and red means \"not ready\".";
		result.getAccessibleContext().setAccessibleDescription(desc);
		result.setToolTipText(desc);
		return result;
	}

	private static JPanel createStatusPanel(final GameBoardPanel<?> boardPanel, final Random rnd, final Dimension gameBoardPanelSize, final Component sidePanel,
			final RemoteController<?> controller) {
		final JPanel result = new JPanel();
		final BoxLayout layout = new BoxLayout(result, BoxLayout.LINE_AXIS);
		result.setLayout(layout);

		final int marginSize = 2;
		final Dimension marginDim = new Dimension(marginSize, 0);

		final Component sidePanelLeftMargin = Box.createRigidArea(marginDim);
		result.add(sidePanelLeftMargin);

		result.add(sidePanel);

		final Component sidePanelRightMargin = Box.createRigidArea(marginDim);
		result.add(sidePanelRightMargin);
		
		final JPanel buttonPanel = new JPanel();
		result.add(buttonPanel);
		final JButton undoButton = new JButton("undo");
		buttonPanel.add(undoButton);
		undoButton.addActionListener(continueEvent -> {
			boardPanel.notifyUndo();
		});
		final JButton continueButton = new JButton("continue");
		buttonPanel.add(continueButton);
		continueButton.addActionListener(continueEvent -> {
			boardPanel.notifyContinue(rnd);
		});

		return result;
	}

	private static TurnLabel createTurnLabel(final PlayerTurnStatus initialStatus) {
		final TurnLabel result = new TurnLabel(initialStatus);
		final Font font = Font.getFont(createTurnLabelFontAttrMap());
		result.setFont(font);
		return result;
	}

	private static Map<Attribute, Object> createTurnLabelFontAttrMap() {
		final Map<Attribute, Object> result = Maps.newHashMapWithExpectedSize(3);
		result.put(TextAttribute.FAMILY, Font.SANS_SERIF);
		result.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD);
		result.put(TextAttribute.SIZE, 36.0f);
		return result;
	}

	private static PlayerTurnStatus getPlayerTurnStatus(final boolean ready) {
		return ready ? PlayerTurnStatus.READY : PlayerTurnStatus.NOT_READY;
	}

	private final Supplier<String> playerIdGetter;
	private final Set<Window> childWindows = new HashSet<>();
	private final ReadinessIndicator playerReadiness;

	private final TurnLabel turnLabel;

	private final MoveCounterLabel moveCounterLabel;

	GameViewFrame(final GameBoardPanel<?> boardPanel, final Random rnd, final LocalController<Integer> localController,
			final RemoteController<Integer> remoteController, final Runnable closeHook) {
		localController.addObserver(this);
		remoteController.addObserver(this);
		playerIdGetter = localController::getPlayerId;
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		addWindowListener(new WindowAdapter() {

			/*
			 * (non-Javadoc)
			 *
			 * @see java.awt.event.WindowAdapter#windowClosed(java.awt.event.
			 * WindowEvent)
			 */
			@Override
			public void windowClosed(final WindowEvent e) {
				childWindows.forEach(childWindow -> childWindow.dispose());
				closeHook.run();
			}

		});
		setLayout(new BorderLayout());
		add(boardPanel, BorderLayout.CENTER);

		final PlayerTurnStatus initialTurnStatus = getPlayerTurnStatus(localController.getRoles().contains(PlayerRole.MOVER));
		turnLabel = createTurnLabel(initialTurnStatus);
		{
			final JPanel turnLabelPanel = new JPanel();
			turnLabelPanel.add(turnLabel);
			add(turnLabelPanel, BorderLayout.PAGE_START);
		}

		final Dimension gameBoardPanelSize = boardPanel.getPreferredSize();
		playerReadiness = createPlayerReadinessIndicator(gameBoardPanelSize.width, initialTurnStatus);
		final JPanel sidePanel;
		{
			sidePanel = new JPanel();
			final BoxLayout layout = new BoxLayout(sidePanel, BoxLayout.PAGE_AXIS);
			sidePanel.setLayout(layout);
			sidePanel.add(playerReadiness);

			moveCounterLabel = createMoveCounterLabel(localController.getMoveCount(),
					playerReadiness.getPreferredSize().width);
			remoteController.addObserver(moveCounterLabel);
			sidePanel.add(moveCounterLabel);
		}
		add(createStatusPanel(boardPanel, rnd, gameBoardPanelSize, sidePanel, remoteController), BorderLayout.PAGE_END);

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable o, final Object arg) {
		if (arg instanceof ActivePlayerChange) {
			LOGGER.debug("Observed event representing a change in the currently-active player.");
			final ActivePlayerChange change = (ActivePlayerChange) arg;
			final String playerId = playerIdGetter.get();
			if (playerId.equals(change.getOldInstructingPlayerId())) {
				// This client initiated the handover
				final String newInstructingPlayerId = change.getNewInstructingPlayerId();
				if (playerId.equals(newInstructingPlayerId)) {
					// No change in active player
				} else {
					// Some other client's user is now active
					LOGGER.debug("Player \"{}\" is now active; Changing view to show the local player is not ready.",
							newInstructingPlayerId);
					setPlayerReady(false);
				}
			} else {
				// Some other client initiated the handover
				if (playerId.equals(change.getNewInstructingPlayerId())) {
					// This client's user is now active
					LOGGER.debug(
							"Foreign player \"{}\" is no longer active; Changing view to show the local player is now ready.",
							playerId);
					setPlayerReady(true);
				} else {
					// Some other client's user is now active
					// TODO: Add notification of 3rd user now being active
					LOGGER.debug("Changing view to show the local player is not ready.");
					setPlayerReady(false);
				}
			}

		} else if (arg instanceof GameEnding) {
			LOGGER.debug("Observed event representing a game ending.");
			final GameEnding ending = (GameEnding) arg;
			final GameEnding.Outcome outcome = ending.getOutcome();
			switch (outcome) {
			case ABORT:
				JOptionPane.showMessageDialog(this, String.format("The game was aborted by \"%s\" after %d move(s).",
						ending.getPlayerId(), ending.getMoveCount()), "Aborted!", JOptionPane.WARNING_MESSAGE);
				// setEnabled(false);
				break;
			case WIN: {
				JOptionPane.showMessageDialog(this,
						String.format("The game was won after %d move(s).", ending.getMoveCount()), "Win!",
						JOptionPane.INFORMATION_MESSAGE);
				// setEnabled(false);
				break;
			}
			default:
				throw new AssertionError(String.format("No logic for handling outcome %s.", outcome));
			}

		} else if (arg instanceof PlayerJoinTime) {
			LOGGER.debug("Observed event representing the joining of a player to the game.");
			JOptionPane.showMessageDialog(this,
					String.format("Player \"%s\" joined the game.", ((PlayerJoinTime) arg).getPlayerId()),
					"Player joined", JOptionPane.INFORMATION_MESSAGE);
		} else {
			LOGGER.debug("Ignoring observed event arg object of type \"{}\".", arg.getClass().getName());
		}
	}

	private void setPlayerReady(final boolean ready) {
		final PlayerTurnStatus newTurnStatus = getPlayerTurnStatus(ready);
		LOGGER.debug("Setting player readiness indicator status to {}.", newTurnStatus);
		playerReadiness.setStatus(newTurnStatus);
		turnLabel.setStatus(newTurnStatus);
	}

	/**
	 * @return the childWindows
	 */
	Set<Window> getChildWindows() {
		return childWindows;
	}

}

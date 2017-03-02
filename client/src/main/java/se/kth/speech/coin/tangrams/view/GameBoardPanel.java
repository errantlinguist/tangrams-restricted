/*
 *  This file is part of tangrams.
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
package se.kth.speech.coin.tangrams.view;

import java.awt.Component;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Observable;
import java.util.Observer;
import java.util.function.BiConsumer;
import java.util.function.Supplier;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.awt.DisablingMouseAdapter;
import se.kth.speech.coin.tangrams.game.LocalController;
import se.kth.speech.coin.tangrams.iristk.events.ActivePlayerChange;
import se.kth.speech.coin.tangrams.iristk.events.GameEnding;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 16 Nov 2016
 *
 */
public final class GameBoardPanel extends JPanel implements Observer {

	private class LabelMovingMouseListenerHandler implements MouseListener.CallbackHandler {

		@Override
		public boolean confirmMove(final int[] sourceCoords, final int[] targetCoords) {
			final boolean result;

			final int response = MoveDialogs.showMoveConfirmDialog(GameBoardPanel.this);
			switch (response) {
			case JOptionPane.YES_OPTION: {
				result = true;
				break;
			}
			case JOptionPane.NO_OPTION: {
				result = false;
				break;
			}
			default: {
				throw new AssertionError(String.format("No logic defined for dialog response %d.", response));
			}

			}

			return result;
		}

		@Override
		public Component findComponent(final int x, final int y) {
			return positionGrid.findComponentAt(x, y);
		}

		@Override
		public void notifyTargetNonAdjacent() {
			MoveDialogs.showTargetNonAdjacentErrorMessage(GameBoardPanel.this);
		}

		@Override
		public void notifyTargetOccupied() {
			MoveDialogs.showTargetOccupiedErrorMessage(GameBoardPanel.this);
		}

		@Override
		public void selectPiece(final JLabel selectedPieceLabel) {
			layeredPane.add(selectedPieceLabel, JLayeredPane.DRAG_LAYER);
		}
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(GameBoardPanel.class);

	/**
	 *
	 */
	private static final long serialVersionUID = -3759792731112624486L;

	private final JLayeredPane layeredPane;

	private final BiConsumer<? super GameBoardPanel, ? super Selection> localSelectionHook;

	private final BiConsumer<? super GameBoardPanel, ? super Turn> localTurnCompletionHook;

	private final List<DisablingMouseAdapter> mouseListeners = new ArrayList<>();

	private final Supplier<String> playerIdGetter;

	private final ModelCoordinateGridPanel<?> positionGrid;

	public <T> GameBoardPanel(final BiConsumer<? super GameBoardPanel, ? super Turn> localTurnCompletionHook,
			final BiConsumer<? super GameBoardPanel, ? super Selection> localSelectionHook,
			final ModelCoordinateGridPanel<?> positionGrid, final Dimension boardSize,
			final LocalController<T> localController) {
		this(localTurnCompletionHook, localSelectionHook, localController::getPlayerId, boardSize, positionGrid);
		registerController(localController);
	}

	public <T> GameBoardPanel(final BiConsumer<? super GameBoardPanel, ? super Turn> localTurnCompletionHook,
			final BiConsumer<? super GameBoardPanel, ? super Selection> localSelectionHook,
			final Supplier<String> playerIdGetter, final Dimension boardSize,
			final ModelCoordinateGridPanel<?> positionGrid) {
		this.playerIdGetter = playerIdGetter;
		this.localTurnCompletionHook = localTurnCompletionHook;
		this.localSelectionHook = localSelectionHook;

		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

		layeredPane = new JLayeredPane();
		layeredPane.setPreferredSize(boardSize);
		add(layeredPane);

		this.positionGrid = positionGrid;
		layeredPane.add(positionGrid, JLayeredPane.DEFAULT_LAYER);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
	 */
	@Override
	public void update(final Observable o, final Object arg) {
		final Stream<?> args;
		if (arg instanceof Stream<?>) {
			args = (Stream<?>) arg;
		} else if (arg instanceof Collection<?>) {
			args = ((Collection<?>) arg).stream();
		} else if (arg instanceof Iterable<?>) {
			final Iterable<?> iter = (Iterable<?>) arg;
			args = StreamSupport.stream(iter.spliterator(), false);
		} else {
			args = Stream.of(arg);
		}
		synchronized (this) {
			// Synchronize once for the whole set of events so that multiple
			// event streams don't interleave in cases where this is still
			// processing one set when another update notification is received
			args.forEach(this::handleUpdateArg);
		}
	}

	private DisablingMouseAdapter createMouseListener(final LocalController<?> localController) {
		return new DisablingMouseAdapter(new MouseListener(localController, new LabelMovingMouseListenerHandler()));
	}

	private boolean equalsPlayerId(final String str) {
		return Objects.equals(playerIdGetter.get(), str);
	}

	private void handleUpdateArg(final Object arg) {
		if (arg instanceof ActivePlayerChange) {
			LOGGER.debug("Observed event representing a change in the currently-active player.");
			final ActivePlayerChange change = (ActivePlayerChange) arg;
			if (equalsPlayerId(change.getOldActivePlayerId())) {
				// This client initiated the handover
				final String newActivePlayerId = change.getNewActivePlayerId();
				if (equalsPlayerId(newActivePlayerId)) {
					// No change in active player
					LOGGER.debug("Player \"{}\" is still active; Ignoring update event.", newActivePlayerId);
				} else {
					// Some other client's user is now active
					LOGGER.debug("Player \"{}\" is now active; Disabling mouse listeners.", newActivePlayerId);
					// JOptionPane.showMessageDialog(this, String.format("%s's
					// turn.", newActivePlayerId));
					setMouseEnabled(false);
				}
			} else {
				// Some other client initiated the handover
				final String newActivePlayerId = change.getNewActivePlayerId();
				if (equalsPlayerId(newActivePlayerId)) {
					// This client's user is now active
					LOGGER.debug("The local player (\"{}\") is now active; Enabling mouse listeners.",
							newActivePlayerId);
					// JOptionPane.showMessageDialog(this, "Your turn!");
					setMouseEnabled(true);
				} else {
					// Some other client's user is now active
					LOGGER.debug("Player \"{}\" is now active; Disabling mouse listeners.", newActivePlayerId);
					// JOptionPane.showMessageDialog(this,
					// String.format("\"%s\"'s turn.", newActivePlayerId));
					setMouseEnabled(false);
					// TODO: Add notification of 3rd user now being active
				}
			}

		} else if (arg instanceof GameEnding) {
			LOGGER.debug("Observed event representing a game ending.");
			final GameEnding ending = (GameEnding) arg;
			final GameEnding.Outcome outcome = ending.getOutcome();
			switch (outcome) {
			case ABORT:
				setMouseEnabled(false);
				break;
			case WIN: {
				setMouseEnabled(false);
				break;
			}
			default:
				throw new AssertionError(String.format("No logic for handling outcome %s.", outcome));
			}

		} else if (arg instanceof Selection) {
			LOGGER.debug("Observed event representing a user selection.");
			// Highlight coordinates
			final Selection selection = (Selection) arg;
			final int[] coords = selection.getCoords().getCoords();
			final String selectingPlayerId = selection.getPlayerId();
			LOGGER.debug("Toggling highlighting of coordinates {} for \"{}\".",
					new Object[] { Arrays.toString(coords), selectingPlayerId });
			final ModelCoordinatePanel<?> gridPanel = positionGrid.getCoordPanel(coords);
			if (gridPanel.toggleHighlighted(selectingPlayerId) && equalsPlayerId(selectingPlayerId)) {
				localSelectionHook.accept(this, selection);
			}

		} else if (arg instanceof Turn) {
			final Turn turn = (Turn) arg;
			final String turnPlayerId = turn.getPlayerId();
			LOGGER.debug("Observed event representing a turn completed by \"{}\".", turnPlayerId);
			if (equalsPlayerId(turnPlayerId)) {
				LOGGER.debug("Skipping view update for remote notification about player's own turn.");
				localTurnCompletionHook.accept(this, turn);
			} else {
				final Move move = turn.getMove();
				updatePieceLabels(move);
			}

		} else {
			LOGGER.debug("Ignoring observed event arg object of type \"{}\".", arg.getClass().getName());
		}
	}

	private void registerController(final LocalController<?> localController) {
		localController.addObserver(this);

		final DisablingMouseAdapter mouseListener = createMouseListener(localController);
		mouseListener.setEnabled(localController.isEnabled());
		addDisablingMouseListener(mouseListener);
	}

	private void updatePieceLabels(final Move move) {
		final int[] sourceCoords = move.getSource().getCoords();
		final ModelCoordinatePanel<?> sourceGridPanel = positionGrid.getCoordPanel(sourceCoords);
		final Component[] components = sourceGridPanel.getComponents();
		// There should be at most one component of the panel
		assert components.length < 2;
		// At this point, there should always be a label
		if (components.length < 1) {
			throw new IllegalArgumentException(
					String.format("Bad move action refers to a source grid panel with no label: %s", move));
		} else {
			final JLabel pieceLabel = (JLabel) components[0];
			pieceLabel.setVisible(false);
			final int[] targetCoords = move.getTarget().getCoords();
			LOGGER.debug("Moving label at coordinates {} to {}.", Arrays.toString(sourceCoords),
					Arrays.toString(targetCoords));
			final ModelCoordinatePanel<?> targetGridPanel = positionGrid.getCoordPanel(targetCoords);
			sourceGridPanel.remove(pieceLabel);
			targetGridPanel.add(pieceLabel);
			pieceLabel.setVisible(true);
		}
	}

	void addDisablingMouseListener(final DisablingMouseAdapter mouseListener) {
		layeredPane.addMouseListener(mouseListener);
		layeredPane.addMouseMotionListener(mouseListener);
		mouseListeners.add(mouseListener);
	}

	void setMouseEnabled(final boolean enabled) {
		mouseListeners.stream().forEach(listener -> listener.setEnabled(enabled));
	}

}
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

import java.awt.Font;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.text.DefaultCaret;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.speech.coin.tangrams.game.PlayerJoinTime;
import se.kth.speech.coin.tangrams.iristk.events.GameEnding;
import se.kth.speech.coin.tangrams.iristk.events.Move;
import se.kth.speech.coin.tangrams.iristk.events.Selection;
import se.kth.speech.coin.tangrams.iristk.events.Turn;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 23 Nov 2016
 *
 */
final class GameStatusScrollPane extends JScrollPane implements Observer {

	private static final ThreadLocal<SimpleDateFormat> DATE_FORMAT = new ThreadLocal<SimpleDateFormat>() {

		/*
		 * (non-Javadoc)
		 *
		 * @see java.lang.ThreadLocal#initialValue()
		 */
		@Override
		protected SimpleDateFormat initialValue() {
			return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		}

	};

	private static final Logger LOGGER = LoggerFactory.getLogger(GameStatusScrollPane.class);

	/**
	 *
	 */
	private static final long serialVersionUID = 4661729605486792445L;

	private static String createMoveStatusMessage(final int moveCount, final String playerId, final Move move) {
		return String.format("Move %d:\t\"%s\" submitted move from %s to %s.", moveCount, playerId,
				Arrays.toString(move.getSource().getCoords()), Arrays.toString(move.getTarget().getCoords()));
	}

	private static JTextPane createStatusPane(final Font font) {
		final JTextPane result = new JTextPane();
		result.setFont(font);
		result.setEditable(false);
		final String desc = "In this area, game status messages are displayed.";
		result.getAccessibleContext().setAccessibleDescription(desc);
		result.setToolTipText(desc);
		// See <http://stackoverflow.com/a/2483824/1391325>
		final DefaultCaret caret = (DefaultCaret) result.getCaret();
		caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
		return result;
	}

	private boolean isStatusPaneFresh = true;

	private final JTextPane statusPane;

	GameStatusScrollPane(final Font font) {
		super(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		statusPane = createStatusPane(font);
		setViewportView(statusPane);
	}

	@Override
	public void update(final Observable o, final Object arg) {
		if (arg instanceof GameEnding) {
			LOGGER.debug("Observed event representing a game ending.");
			final GameEnding ending = (GameEnding) arg;
			final GameEnding.Outcome outcome = ending.getOutcome();

			final String msg;
			switch (outcome) {
			case ABORT: {
				msg = String.format("END:\t\"%s\" The game was aborted after %d move(s).", ending.getMoveCount());
				break;
			}
			case WIN: {
				msg = String.format("END:\tThe game was won after %d move(s).", ending.getMoveCount());
				break;
			}
			default:
				throw new AssertionError(String.format("No logic for handling outcome %s.", outcome));
			}
			updateStatus(msg);
		} else if (arg instanceof PlayerJoinTime) {
			LOGGER.debug("Observed event representing the joining of a player to the game.");
			final PlayerJoinTime playerJoinTime = (PlayerJoinTime) arg;
			final String msg = String.format("Player \"%s\" joined the game at %s.", playerJoinTime.getPlayerId(),
					DATE_FORMAT.get().format(new Date(playerJoinTime.getJoinTime())));
			updateStatus(msg);
		} else if (arg instanceof Selection) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("Observed event representing a user selection.");
				final Selection selection = (Selection) arg;
				final String msg = String.format("DEBUG:\tToggling highlighting for coords %s.",
						Arrays.toString(selection.getCoords().getCoords()));
				updateStatus(msg);
			}
		} else if (arg instanceof Turn) {
			final Turn turn = (Turn) arg;
			final String turnPlayerId = turn.getPlayerId();
			LOGGER.debug("Observed event representing a turn completed by \"{}\".", turnPlayerId);
			final String msg = createMoveStatusMessage(turn.getSequenceNumber(), turnPlayerId, turn.getMove());
			updateStatus(msg);
		} else {
			LOGGER.debug("Ignoring observed event of type \"{}\".", o.getClass().getName());
		}
	}

	private void updateStatus(final String msg) {
		if (isStatusPaneFresh) {
			// FIXME: For some reason, updating the status pane causes the
			// client to hang
			// statusPane.setText("(status disabled due to weird bug)");
			statusPane.setText(msg);
			isStatusPaneFresh = false;
		} else {
			statusPane.setText(statusPane.getText() + System.lineSeparator() + msg);
		}
	}

}

/*
 *  This file is part of se.kth.speech.coin.tangrams.playback.
 *
 *  se.kth.speech.coin.tangrams.playback is free software: you can redistribute it and/or modify
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
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.sql.Timestamp;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.NavigableMap;
import java.util.NoSuchElementException;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.system.Event;
import se.kth.speech.Durations;
import se.kth.speech.MapEntryRemapping;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.analysis.GameHistory;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.iristk.GameEventReplayer;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 26 Jan 2017
 *
 */
final class PlaybackFrame extends BasicGameViewFrame {

	private static final Logger LOGGER = LoggerFactory.getLogger(PlaybackFrame.class);

	/**
	 *
	 */
	private static final long serialVersionUID = 4991235485542157057L;

	private static final ZoneId TIMEZONE = ZoneId.systemDefault();

	private final JTextField currentTimeLabel;

	private final GameEventReplayer eventReplayer;

	private final List<Runnable> fileOpeningHooks = new ArrayList<>();

	private final Instant gameStart;

	/**
	 * @param boardPanel
	 * @param controller
	 * @param moveFactory
	 * @param preferredSize
	 */
	public PlaybackFrame(final AbstractGameBoardPanel boardPanel, final Controller controller,
			final Supplier<? extends MapEntryRemapping<Integer, SpatialRegion>> moveFactory,
			final Dimension preferredSize, final GameHistory gameHistory) {
		super(boardPanel, controller, moveFactory, preferredSize);
		setTitle("Playback");
		setJMenuBar(createMenuBar());

		{
			final NavigableMap<Timestamp, List<Event>> timedEventMap = gameHistory.getEvents();
			final List<Event> events = timedEventMap.values().stream().flatMap(List::stream)
					.collect(Collectors.toList());
			eventReplayer = new GameEventReplayer(events, gameHistory.getStartTime(), controller);
		}
		{
			gameStart = gameHistory.getStartTime().toInstant();
			LOGGER.debug("Set game start as \"{}\".", gameStart);
			currentTimeLabel = new JTextField(Durations.formatDuration(getGameDuration(gameStart)));
			currentTimeLabel.addActionListener(actionEvent -> {
				final String inputDurationStr = currentTimeLabel.getText();
				try {
					final Duration inputDuration = Durations.parseDuration(inputDurationStr);
					final Timestamp inputTimestamp = getTimestamp(inputDuration);
					LOGGER.debug("Timestamp for user input: {}", inputTimestamp);
					final Timestamp currentTime = eventReplayer.getCurrentTime();
					LOGGER.debug("Event replayer's current time: {}", currentTime);
					final int cmp = currentTime.compareTo(inputTimestamp);
					if (cmp < 0) {
						eventReplayer.updateTo(inputTimestamp);
					} else if (cmp > 0) {
						eventReplayer.undoTo(inputTimestamp);
					}

				} catch (final DateTimeParseException ex) {
					JOptionPane.showMessageDialog(PlaybackFrame.this, "Invalid timestamp format entered.",
							"Invalid timestamp", JOptionPane.ERROR_MESSAGE);
				}
				// Update outside of the try block in order to re-set the field
				// in the case of bad inputs
				final Timestamp currentTimestamp = eventReplayer.getCurrentTime();
				final Instant currentInstant = currentTimestamp.toInstant();
				final Duration currentGameDuration = getGameDuration(currentInstant);
				currentTimeLabel.setText(Durations.formatDuration(currentGameDuration));
			});
		}

		final JPanel controlPanel = new JPanel();
		add(controlPanel, BorderLayout.PAGE_END);
		{
			final String previousEventActionCmd = "previous";
			final JButton previousEventButton = new JButton(previousEventActionCmd);
			controlPanel.add(previousEventButton);
			previousEventButton.addActionListener(actionEvent -> {
				// Rewind one event
				try {
					eventReplayer.undoLastEvent();
					final Timestamp eventTime = eventReplayer.getCurrentTime();
					final Instant eventInstant = eventTime.toInstant();
					currentTimeLabel.setText(Durations.formatDuration(getGameDuration(eventInstant)));
				} catch (final NoSuchElementException ex) {
					JOptionPane.showMessageDialog(this, "No more events available in history.", "Beginning reached",
							JOptionPane.WARNING_MESSAGE);
				}

			});
		}
		controlPanel.add(currentTimeLabel);
		{
			final String nextEventActionCmd = "next";
			final JButton nextEventButton = new JButton(nextEventActionCmd);
			controlPanel.add(nextEventButton);
			nextEventButton.addActionListener(actionEvent -> {
				// Go forward one event
				try {
					eventReplayer.applyNextEvent();
					final Timestamp eventTime = eventReplayer.getCurrentTime();
					final Instant eventInstant = eventTime.toInstant();
					currentTimeLabel.setText(Durations.formatDuration(getGameDuration(eventInstant)));
				} catch (final NoSuchElementException e) {
					JOptionPane.showMessageDialog(this, "No more events available in history.", "End reached",
							JOptionPane.WARNING_MESSAGE);
				}
			});
		}

		setResizable(false);
	}

	/**
	 * @return the fileOpeningHooks
	 */
	public List<Runnable> getFileOpeningHooks() {
		return fileOpeningHooks;
	}

	private JMenuItem createFileMenuItem(final Component parent) {
		final JMenu result = new JMenu("File");
		result.setMnemonic(KeyEvent.VK_F);
		final String desc = "File management";
		result.getAccessibleContext().setAccessibleDescription(desc);
		result.setToolTipText(desc);
		final JMenuItem openItem = new JMenuItem("Open");
		result.add(openItem);
		{
			final int viewMenuShortcut = KeyEvent.VK_O;
			openItem.setMnemonic(viewMenuShortcut);
			openItem.setAccelerator(KeyStroke.getKeyStroke(viewMenuShortcut, InputEvent.CTRL_MASK));
		}
		final String openItemDesc = "Show the winning board configuration.";
		openItem.getAccessibleContext().setAccessibleDescription(openItemDesc);
		openItem.setToolTipText(openItemDesc);
		final String openActionCmd = "Open file";
		openItem.setActionCommand(openActionCmd);
		openItem.addActionListener(e -> {
			final String actionCmd = e.getActionCommand();
			switch (actionCmd) {
			case openActionCmd: {
				fileOpeningHooks.stream().forEach(Runnable::run);
				break;
			}
			default: {
				throw new AssertionError(String.format("No logic for handling action command \"%s\".", actionCmd));
			}
			}
		});

		return result;
	}

	private JMenuBar createMenuBar() {
		final JMenuBar result = new JMenuBar();

		result.add(createFileMenuItem(result));

		return result;
	}

	private Duration getGameDuration(final Instant lastInstant) {
		return Duration.between(gameStart, lastInstant);
	}

	private Timestamp getTimestamp(final Duration gameDuration) {
		LOGGER.debug("Creating timestamp for duration \"{}\".", Durations.formatDuration(gameDuration));
		final Instant lastInstant = gameStart.plus(gameDuration);
		LOGGER.debug("Created last instant for given duration: {}", lastInstant);
		final LocalDateTime lastTime = LocalDateTime.ofInstant(lastInstant, TIMEZONE);
		LOGGER.debug("Created LocalDateTime: {}", lastTime);
		return Timestamp.valueOf(lastTime);
	}

}

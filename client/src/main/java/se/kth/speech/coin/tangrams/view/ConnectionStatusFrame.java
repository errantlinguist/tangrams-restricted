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
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator.Attribute;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import javax.swing.JFrame;
import javax.swing.JLabel;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 11 Jan 2017
 *
 */
public final class ConnectionStatusFrame extends JFrame {

	public enum Status {
		CONNECTED, NOT_CONNECTED;
	}

	/**
	 *
	 */
	private static final long serialVersionUID = 371624346692039531L;

	private final JLabel label;

	private Status status;

	private final Map<Status, String> statusLabels;

	public ConnectionStatusFrame(final String gameId, final Status initialStatus,
			final Map<Status, ? extends Consumer<? super ConnectionStatusFrame>> closeHooks) {
		super("Waiting for connection");
		if (closeHooks.size() != Status.values().length) {
			throw new IllegalArgumentException(
					"Close hook map is not the same size as the set of all possible connection states.");
		}
		status = initialStatus;

		statusLabels = new EnumMap<>(Status.class);
		statusLabels.put(Status.CONNECTED, "Connected!");
		statusLabels.put(Status.NOT_CONNECTED, String.format("Connecting to game \"%s\"...", gameId));
		assert statusLabels.size() == Status.values().length;

		label = new JLabel(statusLabels.get(initialStatus));
		{
			final Map<Attribute, Object> fontAttrs = new HashMap<>(4, 1.0f);
			fontAttrs.put(TextAttribute.FAMILY, Font.SANS_SERIF);
			fontAttrs.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD);
			fontAttrs.put(TextAttribute.SIZE, 36.0f);
			label.setFont(Font.getFont(fontAttrs));
		}
		add(label);
		setResizable(false);
		pack();

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
				closeHooks.get(status).accept(ConnectionStatusFrame.this);
			}

		});

	}

	/**
	 * @param status
	 *            the status to set
	 */
	public synchronized void setStatus(final Status status) {
		this.status = status;
	}

}

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

import java.awt.Color;
import java.util.EnumMap;
import java.util.Map;

import javax.swing.JLabel;

import se.kth.speech.awt.ColorIcon;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 9 Jan 2017
 *
 */
final class ReadinessIndicator extends JLabel {

	/**
	 *
	 */
	private static final long serialVersionUID = 3650772804036558427L;

	private static final Map<PlayerTurnStatus, Color> STATUS_COLORS = createStatusColorMap();

	private static final Map<PlayerTurnStatus, Color> createStatusColorMap() {
		final Map<PlayerTurnStatus, Color> result = new EnumMap<>(PlayerTurnStatus.class);

		result.put(PlayerTurnStatus.READY, Color.GREEN);
		result.put(PlayerTurnStatus.NOT_READY, Color.RED);
		assert result.size() == PlayerTurnStatus.values().length;

		return result;
	}

	static Color getStatusColor(final PlayerTurnStatus status) {
		return STATUS_COLORS.get(status);
	}

	private PlayerTurnStatus currentStatus;

	private final ColorIcon icon;

	public ReadinessIndicator(final ColorIcon icon, final PlayerTurnStatus initialStatus) {
		super(icon);
		final Color initialColor = icon.getColor();
		if (!STATUS_COLORS.values().contains(initialColor)) {
			throw new IllegalArgumentException(
					String.format("Supplied %s has color %s, which is not a valid status color.",
							ColorIcon.class.getSimpleName(), initialColor));
		}
		this.icon = icon;
		currentStatus = initialStatus;
	}

	synchronized void setStatus(final PlayerTurnStatus newStatus) {
		final PlayerTurnStatus oldStatus = currentStatus;
		currentStatus = newStatus;
		if (!oldStatus.equals(newStatus)) {
			final Color newColor = getStatusColor(newStatus);
			icon.setColor(newColor);
			repaint();
		}
	}

}

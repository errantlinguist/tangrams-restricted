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
package se.kth.speech.awt;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 14 Dec 2016
 *
 */
public final class DisablingMouseAdapter implements MouseListener, MouseWheelListener, MouseMotionListener {

	private static final MouseAdapter NULL_OBJECT = new MouseAdapter() {
		// Do nothing
	};

	private MouseAdapter current;

	private final MouseAdapter decorated;

	private boolean isEnabled = true;

	public DisablingMouseAdapter(final MouseAdapter decorated) {
		this.decorated = decorated;
		current = decorated;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(final MouseEvent e) {
		current.mouseClicked(e);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.awt.event.MouseMotionListener#mouseDragged(java.awt.event.
	 * MouseEvent)
	 */
	@Override
	public void mouseDragged(final MouseEvent e) {
		current.mouseDragged(e);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseEntered(final MouseEvent e) {
		current.mouseEntered(e);

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseExited(final MouseEvent e) {
		current.mouseExited(e);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * java.awt.event.MouseMotionListener#mouseMoved(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseMoved(final MouseEvent e) {
		current.mouseMoved(e);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(final MouseEvent e) {
		current.mousePressed(e);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(final MouseEvent e) {
		current.mouseReleased(e);

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.awt.event.MouseWheelListener#mouseWheelMoved(java.awt.event.
	 * MouseWheelEvent)
	 */
	@Override
	public void mouseWheelMoved(final MouseWheelEvent e) {
		current.mouseWheelMoved(e);
	}

	public void setEnabled(final boolean enabled) {
		isEnabled = enabled;
		updateEnabled();
	}

	private void updateEnabled() {
		if (isEnabled) {
			current = decorated;
		} else {
			current = NULL_OBJECT;
		}
	}

}

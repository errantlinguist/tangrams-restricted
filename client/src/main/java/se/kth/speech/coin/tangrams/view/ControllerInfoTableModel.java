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

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.function.Supplier;

import javax.swing.table.AbstractTableModel;

import se.kth.speech.MutablePair;
import se.kth.speech.SpatialRegion;
import se.kth.speech.coin.tangrams.game.Controller;
import se.kth.speech.coin.tangrams.game.PlayerRole;
import se.kth.speech.coin.tangrams.game.Turn;
import se.kth.speech.coin.tangrams.iristk.events.Move;

final class ControllerInfoTableModel extends AbstractTableModel implements Controller.Listener {

	private static final int CONTROLLER_DATA_ROW = 0;

	/**
	 *
	 */
	private static final long serialVersionUID = -2397331318460560614L;

	private final List<Entry<String, Supplier<Object>>> columns;

	private final Supplier<Object> scoreGetter;

	private final Supplier<Object> turnCountGetter;

	ControllerInfoTableModel(final Controller controller) {
		scoreGetter = controller::getScore;
		turnCountGetter = controller::getTurnCount;
		controller.getListeners().add(this);
		columns = new ArrayList<>(2);
		columns.add(new MutablePair<>("Turns", turnCountGetter));
		columns.add(new MutablePair<>("Score", scoreGetter));
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return columns.size();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.AbstractTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(final int column) {
		return columns.get(column).getKey();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		return 1;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@Override
	public Object getValueAt(final int rowIndex, final int columnIndex) {
		final Object result;
		switch (rowIndex) {
		case CONTROLLER_DATA_ROW: {
			result = columns.get(columnIndex).getValue().get();
			break;
		}
		default: {
			throw new NoSuchElementException("No data for row index: " + rowIndex);
		}
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateNextMove(se.
	 * kth.speech.coin.tangrams.iristk.events.Move)
	 */
	@Override
	public void updateNextMove(final Move move) {
		// Do nothing
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updatePlayerJoined(
	 * java.lang.String, long)
	 */
	@Override
	public void updatePlayerJoined(final String joinedPlayerId, final long time) {
		// Do nothing
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updatePlayerRole(se.
	 * kth.speech.coin.tangrams.game.PlayerRole)
	 */
	@Override
	public void updatePlayerRole(final PlayerRole newRole) {
		// Do nothing
	}

	@Override
	public void updatePlayerSelection(final Integer pieceId, final SpatialRegion region) {
		// Do nothing
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateScore(int)
	 */
	@Override
	public void updateScore(final int score) {
		final int startIdx = -1;
		int updatedColIdx = startIdx;
		for (final ListIterator<Entry<String, Supplier<Object>>> colIter = columns.listIterator(); colIter.hasNext();) {
			final int colIdx = colIter.nextIndex();
			if (scoreGetter.equals(colIter.next().getValue())) {
				updatedColIdx = colIdx;
				break;
			}
		}
		assert updatedColIdx > startIdx;
		fireTableCellUpdated(CONTROLLER_DATA_ROW, updatedColIdx);
	}

	@Override
	public void updateSelectionRejected(final Integer pieceId, final SpatialRegion region) {
		// Do nothing
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateTurnCompleted(
	 * se.kth.speech.coin.tangrams.game.Turn)
	 */
	@Override
	public void updateTurnCompleted(final Turn turn) {
		// Do nothing
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * se.kth.speech.coin.tangrams.game.Controller.Listener#updateTurnCount(int)
	 */
	@Override
	public void updateTurnCount(final int newCount) {
		final int startIdx = -1;
		int updatedColIdx = startIdx;
		for (final ListIterator<Entry<String, Supplier<Object>>> colIter = columns.listIterator(); colIter.hasNext();) {
			final int colIdx = colIter.nextIndex();
			if (turnCountGetter.equals(colIter.next().getValue())) {
				updatedColIdx = colIdx;
				break;
			}
		}
		assert updatedColIdx > startIdx;
		fireTableCellUpdated(CONTROLLER_DATA_ROW, updatedColIdx);
	}

}
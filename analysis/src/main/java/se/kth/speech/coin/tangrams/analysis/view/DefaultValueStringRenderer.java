/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
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
package se.kth.speech.coin.tangrams.analysis.view;

import java.util.Objects;

import javax.swing.table.DefaultTableCellRenderer;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 11 Aug 2017
 *
 */
final class DefaultValueStringRenderer extends DefaultTableCellRenderer {

	/**
	 *
	 */
	private static final long serialVersionUID = 2499397457989859243L;

	private final String nullValueRepr;

	DefaultValueStringRenderer(final String nullValueRepr) {
		this.nullValueRepr = nullValueRepr;
	}

	@Override
	public void setValue(final Object value) {
		// http://docs.oracle.com/javase/tutorial/uiswing/components/table.html#renderer
		setText(Objects.toString(value, nullValueRepr));
	}

}

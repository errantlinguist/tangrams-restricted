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
package se.kth.speech;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 27 Mar 2017
 *
 */
public final class MapEntryRemapping<K, V> {

	private final K key;

	private final V newValue;

	private final V oldValue;

	public MapEntryRemapping(final K key, final V oldValue, final V newValue) {
		this.key = key;
		this.oldValue = oldValue;
		this.newValue = newValue;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof MapEntryRemapping)) {
			return false;
		}
		final MapEntryRemapping<?, ?> other = (MapEntryRemapping<?, ?>) obj;
		if (key == null) {
			if (other.key != null) {
				return false;
			}
		} else if (!key.equals(other.key)) {
			return false;
		}
		if (newValue == null) {
			if (other.newValue != null) {
				return false;
			}
		} else if (!newValue.equals(other.newValue)) {
			return false;
		}
		if (oldValue == null) {
			if (other.oldValue != null) {
				return false;
			}
		} else if (!oldValue.equals(other.oldValue)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the key
	 */
	public K getKey() {
		return key;
	}

	/**
	 * @return the newValue
	 */
	public V getNewValue() {
		return newValue;
	}

	/**
	 * @return the oldValue
	 */
	public V getOldValue() {
		return oldValue;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (key == null ? 0 : key.hashCode());
		result = prime * result + (newValue == null ? 0 : newValue.hashCode());
		result = prime * result + (oldValue == null ? 0 : oldValue.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(64);
		builder.append("MapEntryRemapping [key=");
		builder.append(key);
		builder.append(", oldValue=");
		builder.append(oldValue);
		builder.append(", newValue=");
		builder.append(newValue);
		builder.append(']');
		return builder.toString();
	}

}

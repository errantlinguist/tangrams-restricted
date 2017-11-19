package se.kth.speech;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public final class PartitionIterator<T> implements Iterator<List<T>> {

	private final int maxPartitionSize;

	private final Iterator<? extends T> wrapped;

	public PartitionIterator(final Iterator<? extends T> wrapped, final int maxPartitionSize) {
		this.wrapped = wrapped;
		this.maxPartitionSize = maxPartitionSize;
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
		if (!(obj instanceof PartitionIterator)) {
			return false;
		}
		final PartitionIterator<?> other = (PartitionIterator<?>) obj;
		if (maxPartitionSize != other.maxPartitionSize) {
			return false;
		}
		if (wrapped == null) {
			if (other.wrapped != null) {
				return false;
			}
		} else if (!wrapped.equals(other.wrapped)) {
			return false;
		}
		return true;
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
		result = prime * result + maxPartitionSize;
		result = prime * result + (wrapped == null ? 0 : wrapped.hashCode());
		return result;
	}

	@Override
	public boolean hasNext() {
		return wrapped.hasNext();
	}

	@Override
	public List<T> next() {
		final List<T> result = new ArrayList<>(maxPartitionSize);
		while (result.size() < maxPartitionSize && wrapped.hasNext()) {
			result.add(wrapped.next());
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(96);
		builder.append("PartitionIterator [wrapped=");
		builder.append(wrapped);
		builder.append(", maxPartitionSize=");
		builder.append(maxPartitionSize);
		builder.append("]");
		return builder.toString();
	}

}
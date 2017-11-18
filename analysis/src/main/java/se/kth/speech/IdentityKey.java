/**
 *
 */
package se.kth.speech;

import java.util.Map;
import java.util.function.Supplier;

/**
 * A wrapper for objects used as keys in a {@link Map} so that they are compared
 * for equality only according to their identity, i.e.&nbsp;so that two objects
 * are equivalent iff they are identical.
 *
 * @param <T>
 *            The type of the object to wrap.
 *
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 18 Nov 2017
 *
 */
public final class IdentityKey<T> implements Supplier<T> {

	/**
	 * The object wrapped for use as a key.
	 */
	private final T wrapped;

	/**
	 * @param wrapped
	 *            The object to wrap for use as a key.
	 */
	public IdentityKey(final T wrapped) {
		this.wrapped = wrapped;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object obj) {
		final boolean result;
		if (this == obj) {
			result = true;
		} else if (obj == null) {
			result = false;
		} else if (obj instanceof IdentityKey) {
			final IdentityKey<?> other = (IdentityKey<?>) obj;
			// Compare by identity rather than equality
			result = this.get() == other.get();
		} else {
			result = false;
		}
		return result;
	}

	@Override
	public T get() {
		return wrapped;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(64);
		builder.append("IdentityEquals [wrapped=");
		builder.append(wrapped);
		builder.append("]");
		return builder.toString();
	}

}

/**
 *
 */
package se.kth.speech.fastutil;

import java.util.Map;

import it.unimi.dsi.fastutil.objects.Object2ObjectLinkedOpenCustomHashMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectLinkedOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenCustomHashMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectCollection;
import it.unimi.dsi.fastutil.objects.ObjectSet;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 18 Nov 2017
 *
 */
public final class TrimmableObject2ObjectMap<K, V> implements Object2ObjectMap<K, V> {

	/**
	 * A closure class for trimming objects implementing the
	 * {@link Object2ObjectMap} interface, i.e.&nbsp;for use with a method reference
	 * to {@link Object2ObjectOpenHashMap#trim()}.
	 *
	 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
	 * @since 18 Nov 2017
	 *
	 */
	public interface Trimmer {
		/**
		 * Trims the given {@link Object2ObjectMap}.
		 *
		 * @return <code>true</code> iff the trimming was successful.
		 */
		boolean call();
	}

	/**
	 * The decorated {@link Object2ObjectMap}.
	 */
	private final Object2ObjectMap<K, V> decorated;

	/**
	 * A {@link Trimmer} which trims (i.e.&nbsp;compacts) the decorated
	 * {@code Object2ObjectMap}.
	 */
	private final Trimmer trimmer;

	/**
	 * @param decorated
	 *            The {@link Object2ObjectLinkedOpenCustomHashMap} to decorate.
	 */
	public TrimmableObject2ObjectMap(final Object2ObjectLinkedOpenCustomHashMap<K, V> decorated) {
		this(decorated, decorated::trim);
	}

	/**
	 * @param decorated
	 *            The {@link Object2ObjectLinkedOpenHashMap} to decorate.
	 */
	public TrimmableObject2ObjectMap(final Object2ObjectLinkedOpenHashMap<K, V> decorated) {
		this(decorated, decorated::trim);
	}

	/**
	 * @param decorated
	 *            The {@link Object2ObjectMap} to decorate.
	 * @param trimmer
	 *            A {@link Trimmer} which trims (i.e.&nbsp;compacts) the decorated
	 *            {@code Object2ObjectMap}.
	 */
	public TrimmableObject2ObjectMap(final Object2ObjectMap<K, V> decorated, final Trimmer trimmer) {
		this.decorated = decorated;
		this.trimmer = trimmer;
	}

	/**
	 * @param decorated
	 *            The {@link Object2ObjectOpenCustomHashMap} to decorate.
	 */
	public TrimmableObject2ObjectMap(final Object2ObjectOpenCustomHashMap<K, V> decorated) {
		this(decorated, decorated::trim);
	}

	/**
	 * @param decorated
	 *            The {@link Object2ObjectOpenHashMap} to decorate.
	 */
	public TrimmableObject2ObjectMap(final Object2ObjectOpenHashMap<K, V> decorated) {
		this(decorated, decorated::trim);
	}

	@Override
	public void clear() {
		decorated.clear();
	}

	@Override
	public boolean containsKey(final Object key) {
		return decorated.containsKey(key);
	}

	@Override
	public boolean containsValue(final Object value) {
		return decorated.containsValue(value);
	}

	@Override
	public V defaultReturnValue() {
		return decorated.defaultReturnValue();
	}

	@Override
	public void defaultReturnValue(final V rv) {
		decorated.defaultReturnValue(rv);
	}

	@Override
	public ObjectSet<Map.Entry<K, V>> entrySet() {
		return decorated.entrySet();
	}

	@Override
	public V get(final Object key) {
		return decorated.get(key);
	}

	@Override
	public boolean isEmpty() {
		return decorated.isEmpty();
	}

	@Override
	public ObjectSet<K> keySet() {
		return decorated.keySet();
	}

	@Override
	public ObjectSet<Entry<K, V>> object2ObjectEntrySet() {
		return decorated.object2ObjectEntrySet();
	}

	@Override
	public V put(final K key, final V value) {
		return decorated.put(key, value);
	}

	@Override
	public void putAll(final Map<? extends K, ? extends V> m) {
		decorated.putAll(m);
	}

	@Override
	public V remove(final Object key) {
		return decorated.remove(key);
	}

	@Override
	public int size() {
		return decorated.size();
	}

	/**
	 * Trims the decorated {@link Object2ObjectMap}.
	 *
	 * @return <code>true</code> iff the trimming was successful.
	 */
	public boolean trim() {
		return trimmer.call();
	}

	@Override
	public ObjectCollection<V> values() {
		return decorated.values();
	}

}

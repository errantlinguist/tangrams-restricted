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
package se.kth.speech.awt;

import java.awt.Color;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 22 Mar 2017
 *
 */
public final class Colors {

	private static final Logger LOGGER = LoggerFactory.getLogger(Colors.class);

	/**
	 * @return
	 * @throws IllegalAccessException
	 * @see <a href=
	 *      "http://stackoverflow.com/a/4126126/1391325">StackOverflow</a>
	 */
	public static <C extends Collection<? super String>> Map<Integer, C> createColorNameMap(
			final Function<? super String, String> nameTransformer, final Supplier<? extends C> nameCollFactory) {
		final Map<Integer, C> result = new HashMap<>();

		for (final Field field : Color.class.getFields()) {
			if (Modifier.isStatic(field.getModifiers()) && Color.class.isAssignableFrom(field.getType())) {
				final String name = field.getName();
				try {
					final Color color = (Color) field.get(null);
					final String transformedName = nameTransformer.apply(name);
					final C names = result.computeIfAbsent(color.getRGB(), key -> nameCollFactory.get());
					names.add(transformedName);
				} catch (final IllegalAccessException e) {
					LOGGER.warn(String.format("A(n) %s occured while accessing field named \"%s\".",
							e.getClass().getSimpleName(), name), e);
				}
			}
		}
		return result;
	}

	private Colors() {

	}

}

/*
 * 	Copyright 2016 Todd Shore
 *
 *	Licensed under the Apache License, Version 2.0 (the "License");
 *	you may not use this file except in compliance with the License.
 *	You may obtain a copy of the License at
 *
 *		http://www.apache.org/licenses/LICENSE-2.0
 *
 *	Unless required by applicable law or agreed to in writing, software
 *	distributed under the License is distributed on an "AS IS" BASIS,
 *	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *	See the License for the specific language governing permissions and
 *	limitations under the License.
 */
package com.github.errantlinguist;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * A utility class for conveniently loading Java properties files named
 * analogously to the {@link Class} they are used to configure, e.g.&nbsp;
 * <code>com/foo/bar/SomeClass.properties</code> for the class
 * <code>com.foo.bar.SomeClass</code>.
 *
 * @author <a href="mailto:errantlinguist+github@gmail.com">Todd Shore</a>
 * @since 2016-12-01
 *
 */
public final class ClassProperties {

	/**
	 * Creates a new {@link Properties} instance and
	 * {@link Properties#load(InputStream) loads} a property file named
	 * analogously to a given {@link Class}.
	 *
	 * @param propertiedClass
	 *            The {@code Class} to load get the relevant properties for.
	 * @return A new {@code Properties} instance loaded with the given class's
	 *         properties.
	 * @throws IOException
	 *             If an error occurred while opening or reading the analogous
	 *             properties file for the given class.
	 */
	public static Properties load(final Class<?> propertiedClass) throws IOException {
		final Properties result = new Properties();

		try (final InputStream inStream = propertiedClass
				.getResourceAsStream(propertiedClass.getSimpleName() + ".properties")) {
			result.load(inStream);
		}

		return result;
	}

	private ClassProperties() {
	}

}

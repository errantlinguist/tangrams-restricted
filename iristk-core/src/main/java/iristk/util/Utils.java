/*******************************************************************************
 * Copyright (c) 2014 Gabriel Skantze.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 *
 * Contributors:
 *     Gabriel Skantze - initial API and implementation
 ******************************************************************************/
package iristk.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;

public final class Utils {

	public static final Charset IO_CHARSET = StandardCharsets.UTF_8; // NO_UCD
																		// (use
																		// default)

	public static String replaceEnv(final String str) {
		return Replacer.replaceAll(str, "%(.*?)%", (final Matcher m) -> {
			if (System.getenv(m.group(1)) != null) {
				return System.getenv(m.group(1));
			} else {
				return m.group(0);
			}
		});
	}

	/**
	 * Deletes all the contents of the folder (but preserves the empty folder)
	 * 
	 * @param folder
	 *            the folder to clean
	 * @return true if successful
	 */
	private static boolean cleanFolder(final File folder) {
		if (!folder.isDirectory()) {
			return false;
		}
		final File[] files = folder.listFiles();
		boolean success = true;
		if (files != null) {
			for (final File f : files) {
				if (f.isDirectory()) {
					success = deleteFolder(f) && success;
				} else {
					success = f.delete() && success;
				}
			}
		}
		return success;
	}

	/**
	 * Deletes the folder and all of its contents
	 * 
	 * @param folder
	 *            the folder to delete
	 * @return true if successful
	 */
	private static boolean deleteFolder(final File folder) {
		if (!folder.exists()) {
			return false;
		}
		if (!folder.isDirectory()) {
			return false;
		}
		if (!cleanFolder(folder)) {
			return false;
		}
		return folder.delete();
	}

	private static String readString(final InputStream in) throws IOException {
		// return IOUtils.toString(in, DEFAULT_CHARSET.name());
		try (java.util.Scanner s = new java.util.Scanner(in, IO_CHARSET.name())) {
			s.useDelimiter("\\A");
			return s.hasNext() ? s.next() : "";
		}
	}

	static String readTextFile(final File file) throws IOException {
		/*
		 * byte[] buffer = new byte[(int) file.length()]; BufferedInputStream is
		 * = new BufferedInputStream(new FileInputStream(file));
		 * is.read(buffer); is.close(); return new String(buffer);
		 */
		return readString(new FileInputStream(file));
	}

	private Utils() {

	}

}

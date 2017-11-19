package se.kth.speech.io;

import java.io.Closeable;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.function.Supplier;

public final class PrintWriterFetcher implements Closeable, Supplier<PrintWriter> {

	private final Charset encoding;

	private final Path outPath;

	private PrintWriter writer;

	public PrintWriterFetcher(final Path outPath, final Charset encoding) {
		this.outPath = outPath;
		this.encoding = encoding;
	}

	@Override
	public void close() {
		if (writer != null) {
			writer.close();
		}
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
		if (!(obj instanceof PrintWriterFetcher)) {
			return false;
		}
		final PrintWriterFetcher other = (PrintWriterFetcher) obj;
		if (encoding == null) {
			if (other.encoding != null) {
				return false;
			}
		} else if (!encoding.equals(other.encoding)) {
			return false;
		}
		if (outPath == null) {
			if (other.outPath != null) {
				return false;
			}
		} else if (!outPath.equals(other.outPath)) {
			return false;
		}
		return true;
	}

	@Override
	public PrintWriter get() {
		if (writer == null) {
			try {
				writer = new PrintWriter(Files.newBufferedWriter(outPath, encoding, StandardOpenOption.CREATE,
						StandardOpenOption.TRUNCATE_EXISTING));
			} catch (final IOException e) {
				throw new UncheckedIOException(e);
			}
		}
		return writer;
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
		result = prime * result + (encoding == null ? 0 : encoding.hashCode());
		result = prime * result + (outPath == null ? 0 : outPath.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder(128);
		builder.append("PrintWriterFetcher [outPath=");
		builder.append(outPath);
		builder.append(", encoding=");
		builder.append(encoding);
		builder.append(", writer=");
		builder.append(writer);
		builder.append("]");
		return builder.toString();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#finalize()
	 */
	@Override
	protected void finalize() throws Throwable {
		try {
			close();
		} finally {
			super.finalize();
		}
	}
}
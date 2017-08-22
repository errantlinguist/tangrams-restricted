/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.client.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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
package se.kth.speech.io;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Arrays;

import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.UnsupportedAudioFileException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class BufferedAudio {

	private static final Logger LOGGER = LoggerFactory.getLogger(BufferedAudio.class);

	public static BufferedAudio read(final URL res, final int bufferSize)
			throws IOException, UnsupportedAudioFileException {
		final BufferedAudio result;
		final AudioFileFormat format = AudioSystem.getAudioFileFormat(res);
		LOGGER.debug("Loading audio from \"{}\".", res);
		try (AudioInputStream instream = AudioSystem.getAudioInputStream(res)) {
			// AudioFormat format = instream.getFormat();
			try (final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(bufferSize)) {
				AudioSystem.write(instream, format.getType(), byteArrayOutputStream);
				final byte[] data = byteArrayOutputStream.toByteArray();
				LOGGER.debug("Finished loading audio from \"{}\".", res);
				result = new BufferedAudio(format, data, bufferSize);
			}
		}
		return result;
	}

	private final int bufferSize;

	private final byte[] data;

	private final AudioFileFormat format;

	public BufferedAudio(final AudioFileFormat format, final byte[] data, final int bufferSize) {
		this.format = format;
		this.data = data;
		this.bufferSize = bufferSize;
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
		if (!(obj instanceof BufferedAudio)) {
			return false;
		}
		final BufferedAudio other = (BufferedAudio) obj;
		if (bufferSize != other.bufferSize) {
			return false;
		}
		if (!Arrays.equals(data, other.data)) {
			return false;
		}
		if (format == null) {
			if (other.format != null) {
				return false;
			}
		} else if (!format.equals(other.format)) {
			return false;
		}
		return true;
	}

	/**
	 * @return the bufferSize
	 */
	public int getBufferSize() {
		return bufferSize;
	}

	/**
	 * @return the data
	 */
	public byte[] getData() {
		return data;
	}

	/**
	 * @return the format
	 */
	public AudioFileFormat getFormat() {
		return format;
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
		result = prime * result + bufferSize;
		result = prime * result + (format == null ? 0 : format.hashCode());
		result = prime * result + Arrays.hashCode(data);
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
		builder.append("BufferedAudio [bufferSize=");
		builder.append(bufferSize);
		builder.append(", format=");
		builder.append(format);
		builder.append("]");
		return builder.toString();
	}
}
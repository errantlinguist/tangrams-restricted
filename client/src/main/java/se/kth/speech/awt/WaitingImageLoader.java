/*
 *  This file is part of client.
 *
 *  se.kth.speech.coin.tangrams.client is free software: you can redistribute it and/or modify
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

import java.awt.Component;
import java.awt.Container;
import java.awt.Image;
import java.awt.MediaTracker;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author <a href="mailto:tcshore@kth.se>Todd Shore</a>
 * @since 8 Mar 2017
 *
 */
public final class WaitingImageLoader implements Function<Image, Image> {

	private final Consumer<? super InterruptedException> interruptHandler;

	private final MediaTracker mt;

	private final AtomicInteger nextId = new AtomicInteger(0);

	private WaitingImageLoader(final Consumer<? super InterruptedException> interruptHandler) {
		this(interruptHandler, new Container());
	}

	private WaitingImageLoader(final Consumer<? super InterruptedException> interruptHandler, final Component mtComp) {
		this(interruptHandler, new MediaTracker(mtComp));
	}

	private WaitingImageLoader(final Consumer<? super InterruptedException> interruptHandler, final MediaTracker mt) {
		this.interruptHandler = interruptHandler;
		this.mt = mt;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.util.function.Function#apply(java.lang.Object)
	 */
	@Override
	public Image apply(final Image img) {
		final int id = nextId.getAndIncrement();
		mt.addImage(img, id);
		try {
			// http://stackoverflow.com/a/42401667/1391325
			mt.waitForID(id);
		} catch (final InterruptedException e) {
			interruptHandler.accept(e);
		}
		return img;
	}

}

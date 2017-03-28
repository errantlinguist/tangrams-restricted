package se.kth.speech.awt;

import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public final class ComponentResizedEventListener extends ComponentAdapter {

	private final Runnable hook;

	public ComponentResizedEventListener(final Runnable hook) {
		this.hook = hook;
	}

	@Override
	public void componentResized(final ComponentEvent e) {
		hook.run();
	}
}
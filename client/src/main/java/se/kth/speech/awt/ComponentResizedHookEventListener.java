package se.kth.speech.awt;

import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public final class ComponentResizedHookEventListener extends ComponentAdapter {

	private final Runnable hook;

	public ComponentResizedHookEventListener(final Runnable hook) {
		this.hook = hook;
	}

	@Override
	public void componentResized(final ComponentEvent e) {
		hook.run();
	}
}
package se.kth.speech.awt;

import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

public final class ComponentResizedEventHookRunningComponentListener extends ComponentAdapter {

	private final Runnable hook;

	public ComponentResizedEventHookRunningComponentListener(final Runnable hook) {
		this.hook = hook;
	}

	@Override
	public void componentResized(final ComponentEvent e) {
		hook.run();
	}
}
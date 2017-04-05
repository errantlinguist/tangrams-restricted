package se.kth.speech.coin.tangrams.iristk;

public final class MyLogger {

	public void debug(final Object string, final Object joinedPlayerId, final Object joinTime) {
		System.out.println(string + " " + joinedPlayerId + " " + joinTime);
	}

	public void debug(final String string) {
		System.out.println(string);
	}

	public void debug(final String string, final Object obj) {
		System.out.println(string + " " + obj);
	}

	public void info(final String string) {
		System.out.println(string);
	}

	public void info(final String string, final Object objects) {
		System.out.println(string + " " + objects);
	}

	public boolean isDebugEnabled() {
		return true;
	}

}

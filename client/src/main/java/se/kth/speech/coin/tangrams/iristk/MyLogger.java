package se.kth.speech.coin.tangrams.iristk;

public class MyLogger {

	public void debug(Object string, Object joinedPlayerId, Object joinTime) {
		System.out.println(string + " " + joinedPlayerId + " " + joinTime);
	}

	public void debug(String string, Object obj) {
		System.out.println(string + " " + obj);
	}

	public boolean isDebugEnabled() {
		return true;
	}

	public void debug(String string) {
		System.out.println(string);
	}

	public void info(String string, Object objects) {
		System.out.println(string + " " + objects);
	}

	public void info(String string) {
		System.out.println(string);
	}

	
}

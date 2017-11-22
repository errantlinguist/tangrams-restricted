package iristk.project;

import java.io.File;
import java.lang.reflect.Field;
import java.util.HashSet;

public class Launcher {
	
	public static HashSet<String> javaLibPaths = new HashSet<>();
	
	public static void addJavaLibPath(File path) {
		if (!javaLibPaths.contains(path.getAbsolutePath())) {
			try {
				System.setProperty("java.library.path", System.getProperty("java.library.path") + ";" + path.getAbsolutePath());
				Field fieldSysPath = ClassLoader.class.getDeclaredField("sys_paths");
				fieldSysPath.setAccessible( true );
				fieldSysPath.set( null, null );
				javaLibPaths.add(path.getAbsolutePath());
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (NoSuchFieldException e) {
				e.printStackTrace();
			} catch (SecurityException e) {
				e.printStackTrace();
			}
		}
	}

	public static boolean is64arch() {
		return System.getProperty("sun.arch.data.model").equals("64");
	}
	
	
}

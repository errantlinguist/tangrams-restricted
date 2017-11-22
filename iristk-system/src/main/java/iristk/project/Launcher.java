package iristk.project;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.tools.Diagnostic;
import javax.tools.Diagnostic.Kind;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import iristk.util.ArgParser;
import iristk.util.ProcessRunner;
import iristk.util.ProcessRunner.ProcessListener;
import iristk.util.Utils;

public class Launcher {
	
	public static HashSet<String> javaLibPaths = new HashSet<>();
	
	public static void javadoc(String[] args) throws IOException {
		Project.main.getPath();
		String outdir = new File(Project.main.getPath(), "doc/javadoc").getAbsolutePath();
		String cp = Project.main.getClasspath();
		String srcpaths = Project.main.getSrcPaths();
		String cmd = "javadoc -Xdoclint:none -windowtitle \"" + Project.main.getName() + " Javadoc\" -d " + outdir + " -quiet -sourcepath \"" + srcpaths + "\" -classpath \"" + cp + "\" -subpackages iristk";
		System.out.println(cmd);
		new ProcessRunner(cmd, System.out, System.err).waitFor();
	}
	
	public static String getEnv(String name) throws Exception {
		String value = ProcessRunner.eval("reg query HKCU\\Environment /v " + name).trim();
		for (String line : value.split("\n")) {
			Matcher m = Pattern.compile(name + " +[^ ]+ +(.*)", Pattern.CASE_INSENSITIVE).matcher(line.trim());
			if (m.matches()){
				return m.group(1);
			}
		}
		return null;
	}

	public static void setEnv(String name, String value) throws Exception {
		//value = value.replace("%", "~");
		//String result = getCmd("reg add HKCU\\Environment /f /v " + name + " /t " + type + " /d \"" + value + "\"").trim();
		String result = ProcessRunner.eval("setx " + name + " \"" + value + "\"").trim();
		if (!result.contains("SUCCESS")) {
			throw new Exception(result);
		}
	}
	
	public static File getExecutable() {
		return new File(System.getProperty("iristk.exefile"));
	}

	public static void listPackages(String[] args) {
		for (Package pack : Project.main.getAllPackages()) {
			System.out.println(Utils.pad(pack.getName(), 20) + pack.getPath());
		}
	}

	private static String getJavaCmd(boolean force32) throws Exception {
		if (force32 && System.getProperty("sun.arch.data.model").equals("64")) {
			File javaDir = new File(System.getenv("ProgramFiles(x86)"),  "Java");
			if (javaDir.exists() && javaDir.isDirectory()) {
				String[] versions = new String[]{"jdk1.8", "jre8", "jre1.8"};
				for (String v : versions) {
					for (File f : javaDir.listFiles()) {
						if (f.getName().startsWith(v)) {
							File javaexe = new File(new File(f.getPath(), "bin"), "java.exe");
							if (javaexe.exists()) {
								return javaexe.getAbsolutePath();
							}
						}
					}
				}
			}
			throw new Exception("Could not find Java 32-bit runtime");
		} else {
		    File javaexe = Paths.get(System.getProperty("java.home"), "bin", "java.exe").toFile();
		    if (!javaexe.exists()) {
		        javaexe = Paths.get(System.getProperty("java.home"), "bin", "java").toFile();
		    }
		    
			if (javaexe.exists()) {
				return javaexe.getAbsolutePath();
			}
			throw new Exception("Could not find Java runtime");
		}
	}
	
	public static void compilePackage(File packagePath) throws Exception {
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		if (ToolProvider.getSystemJavaCompiler() == null) {
			throw new Exception("Could not find Java Compiler");
		}
		File outputDir = new File(packagePath, "bin");
		Utils.deleteFolder(outputDir);
		outputDir.mkdirs();
		DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<JavaFileObject>();
		StandardJavaFileManager fileManager = compiler.getStandardFileManager(diagnostics, Locale.US, null);
		List<String> files = new ArrayList<String>();
		Utils.listFiles(new File(packagePath, "src").getAbsolutePath(), files, ".*\\.java$");
		for (String f: files) {
			System.out.println("Compiling " + f);
		}
		Iterable<? extends JavaFileObject> compilationUnits = fileManager.getJavaFileObjectsFromStrings(files);
		Iterable<String> args = Arrays.asList("-d", outputDir.getAbsolutePath(), "-cp", Project.main.getClasspath());
		JavaCompiler.CompilationTask task = compiler.getTask(null, fileManager, diagnostics, args, null, compilationUnits);
		boolean success = task.call();
		try {
			fileManager.close();
		} catch (IOException e) {
		}
		for (Diagnostic<? extends JavaFileObject> diag : diagnostics.getDiagnostics()) {
			if (diag.getKind() == Kind.ERROR) {
				@SuppressWarnings("unused")
				final int javaLine = (int) diag.getLineNumber();
				String message = diag.getMessage(Locale.US);
				throw new Exception(message);
			}
		}
		if (!success) {
			throw new Exception("Compilation failed for unknown reason");
		}
	}

	public static ProcessRunner runCommand(String name, String... args) throws Exception {
		ArrayList<String> cmd = new ArrayList<String>();
		cmd.add(name);
		for (String a : args){
			cmd.add(a);
		}
		return runJava(false, Launcher.class.getName(), null, cmd.toArray(new String[0]), System.out, System.err, false, null, null);
	}

	public static ProcessRunner runJava(boolean force32, String mainClass, String[] options, String[] args, OutputStream stdout, OutputStream stderr, boolean waitFor, ProcessListener listener, File workingDir) throws Exception {
		List<String> cmd = new ArrayList<>();
		cmd.add(getJavaCmd(force32));
		cmd.add("-cp");
		cmd.add(Project.main.getClasspath());
		cmd.add("-Diristk.project=\"" + Project.main.getPath() + "\"");
		if (options != null){
			for (String op : options) {
				cmd.add(op);
			}
		}
		cmd.add(mainClass);
		if (args != null){
			for (String arg : args) {
				cmd.add(arg);
			}
		}
		//String cmd = "\"" + javaCmd + "\" -cp \"" + IrisUtils.getClasspath() + "\" " + mainClass + " " + StringUtils.join(args, " ");
		ProcessRunner proc = new ProcessRunner(cmd, stdout, stderr, listener, workingDir);
		if (waitFor){
			proc.waitFor();
		}
		return proc;
	}

	public static void checkDepends(String[] args) throws IOException {
		ArgParser parser = new ArgParser();
		parser.addBooleanArg("r", "check recursively");
		parser.parse(args);
		System.out.println("Checking dependencies in " + Project.main.getPath());
		for (Package pack : Project.main.getAllPackages()) {
			List<String> cmd = new ArrayList<>();
			cmd.add("jdeps");
			cmd.add("-cp");
			cmd.add(Project.main.getClasspath());
			cmd.add("-v");
			if ((Boolean)parser.get("r")){
				cmd.add("-R");
			}
			cmd.add(pack.getPath("bin").getAbsolutePath());
			ProcessRunner proc = new ProcessRunner(cmd, null, System.err, new ProcessListener() {

				@Override
				public void processOutput(String line) {
					if (line.contains("not found")){
						System.out.println(line);
					}
				}

				@Override
				public void processDone(int result) {
				}
			}, null);
			proc.waitFor();
		}
	}

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

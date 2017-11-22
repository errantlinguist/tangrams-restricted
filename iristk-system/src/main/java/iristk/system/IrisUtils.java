package iristk.system;
/*******************************************************************************
 * Copyright (c) 2014 Gabriel Skantze.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Public License v3.0
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/gpl.html
 * 
 * Contributors:
 *     Gabriel Skantze - initial API and implementation
 ******************************************************************************/


import java.io.File;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import iristk.project.Launcher;

public class IrisUtils {
	
	private static boolean logInitialized = false;

	private static void initLogger() {
		if (!logInitialized) {
			//Package core = CorePackage.PACKAGE;
			//if (core != null && core.getPath("log4j.properties").exists())
			//	PropertyConfigurator.configure(core.getPath("log4j.properties").getAbsolutePath());
			//else
//			PropertyConfigurator.configure(IrisUtils.class.getResource("log4j.properties"));
			//Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler());
			logInitialized = true;
		}
	}
	
	public static Logger getLogger(Class<?> clazz) {
		initLogger();
		return LoggerFactory.getLogger(clazz);
	}
	
	public static File getTempDir(String name) {
		return new File(System.getProperty("java.io.tmpdir") + File.separator + "iristk" + File.separator + name);
	}

	public static void addCoreLibPath() {
		if (Launcher.is64arch()) {
			Launcher.addJavaLibPath(CorePackage.PACKAGE.getPath("lib/x64"));
		} else {
			Launcher.addJavaLibPath(CorePackage.PACKAGE.getPath("lib/x86"));
		}
	}

}
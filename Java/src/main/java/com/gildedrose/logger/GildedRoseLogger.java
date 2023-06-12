package com.gildedrose.logger;

import java.util.logging.ConsoleHandler;
import java.util.logging.Logger;

public class GildedRoseLogger {
	
	private static final Logger logger = Logger.getLogger(GildedRoseLogger.class.getName());
	
	private GildedRoseLogger() {
	}
	
	static {
		logger.setUseParentHandlers(false);
		ConsoleHandler handler = new ConsoleHandler();
		logger.addHandler(handler);
	}
	
	public static Logger getLogger() {
		return logger;
	}

}

package com.gildedrose.logger;

import java.util.logging.*;

public class CustomLogger {
	private static final Logger logger = Logger.getLogger(CustomLogger.class.getName());

	static {
		logger.setUseParentHandlers(false);
		ConsoleHandler handler = new ConsoleHandler();
		handler.setFormatter(new CustomFormatter());
		logger.addHandler(handler);
	}

	public static Logger getLogger() {
		return logger;
	}

	private CustomLogger() {
	}
}

class CustomFormatter extends Formatter {
	@Override
	public String format(LogRecord logRecord) {
		return logRecord.getMessage() + "\n";
	}
}

package lib

import (
    "fmt"

    "go.uber.org/fx/fxevent"
    "go.uber.org/zap"
    "go.uber.org/zap/zapcore"
)

// Logger structure
type Logger struct {
    *zap.SugaredLogger
}

type GinLogger struct {
    *Logger
}

type FxLogger struct {
    *Logger
}


var (
    globalLogger *Logger
    zapLogger    *zap.Logger
)

// GetLogger get the logger
func GetLogger() Logger {
    if globalLogger == nil {
        logger := newLogger(NewEnv())
        globalLogger = &logger
    }
    return *globalLogger
}

// GetGinLogger get the gin logger
func (this Logger) GetGinLogger() GinLogger {
    logger := zapLogger.WithOptions(
        zap.WithCaller(false),
    )
    return GinLogger{
        Logger: newSugaredLogger(logger),
    }
}

// GetFxLogger gets logger for go-fx
func (this *Logger) GetFxLogger() fxevent.Logger {
    logger := zapLogger.WithOptions(
        zap.WithCaller(false),
    )
    return &FxLogger{Logger: newSugaredLogger(logger)}
}

// LogEvent log event for fx logger
func (this *FxLogger) LogEvent(event fxevent.Event) {
    switch e := event.(type) {
    case *fxevent.OnStartExecuting:
        this.Logger.Debug("OnStart hook executing: ",
            zap.String("callee", e.FunctionName),
            zap.String("caller", e.CallerName),
        )
    case *fxevent.OnStartExecuted:
        if e.Err != nil {
            this.Logger.Debug("OnStart hook failed: ",
                zap.String("callee", e.FunctionName),
                zap.String("caller", e.CallerName),
                zap.Error(e.Err),
            )
        } else {
            this.Logger.Debug("OnStart hook executed: ",
                zap.String("callee", e.FunctionName),
                zap.String("caller", e.CallerName),
                zap.String("runtime", e.Runtime.String()),
            )
        }
    case *fxevent.OnStopExecuting:
        this.Logger.Debug("OnStop hook executing: ",
            zap.String("callee", e.FunctionName),
            zap.String("caller", e.CallerName),
        )
    case *fxevent.OnStopExecuted:
        if e.Err != nil {
            this.Logger.Debug("OnStop hook failed: ",
                zap.String("callee", e.FunctionName),
                zap.String("caller", e.CallerName),
                zap.Error(e.Err),
            )
        } else {
            this.Logger.Debug("OnStop hook executed: ",
                zap.String("callee", e.FunctionName),
                zap.String("caller", e.CallerName),
                zap.String("runtime", e.Runtime.String()),
            )
        }
    case *fxevent.Supplied:
        this.Logger.Debug("supplied: ", zap.String("type", e.TypeName), zap.Error(e.Err))
    case *fxevent.Provided:
        for _, rtype := range e.OutputTypeNames {
            this.Logger.Debug("provided: ", e.ConstructorName, " => ", rtype)
        }
    case *fxevent.Decorated:
        for _, rtype := range e.OutputTypeNames {
            this.Logger.Debug("decorated: ",
                zap.String("decorator", e.DecoratorName),
                zap.String("type", rtype),
            )
        }
    case *fxevent.Invoking:
        this.Logger.Debug("invoking: ", e.FunctionName)
    case *fxevent.Started:
        if e.Err == nil {
            this.Logger.Debug("started")
        }
    case *fxevent.LoggerInitialized:
        if e.Err == nil {
            this.Logger.Debug("initialized: custom fxevent.Logger -> ", e.ConstructorName)
        }
    }
}

func newSugaredLogger(logger *zap.Logger) *Logger {
    return &Logger{
        SugaredLogger: logger.Sugar(),
    }
}

// newLogger sets up logger
func newLogger(env Env) Logger {

    config := zap.NewDevelopmentConfig()
    logOutput := env.LogOutput

    if env.Environment == "development" {
        fmt.Println("encode level")
        config.EncoderConfig.EncodeLevel = zapcore.CapitalColorLevelEncoder
    }

    if env.Environment == "production" && logOutput != "" {
        config.OutputPaths = []string{logOutput}
    }

    logLevel := env.LogLevel
    level := zap.PanicLevel
    switch logLevel {
    case "debug":
        level = zapcore.DebugLevel
    case "info":
        level = zapcore.InfoLevel
    case "warn":
        level = zapcore.WarnLevel
    case "error":
        level = zapcore.ErrorLevel
    case "fatal":
        level = zapcore.FatalLevel
    default:
        level = zap.PanicLevel
    }
    config.Level.SetLevel(level)

    zapLogger, _ = config.Build()
    logger := newSugaredLogger(zapLogger)

    return *logger
}

// Write interface implementation for gin-framework
func (this GinLogger) Write(p []byte) (n int, err error) {
    this.Info(string(p))
    return len(p), nil
}

// Printf prits go-fx logs
func (this FxLogger) Printf(str string, args ...interface{}) {
    if len(args) > 0 {
        this.Debugf(str, args)
    }
    this.Debug(str)
}

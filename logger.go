package main

import "log/slog"

type LogLevel slog.Level

const (
	LogLevelDebug LogLevel = LogLevel(slog.LevelDebug)
	LogLevelInfo  LogLevel = LogLevel(slog.LevelInfo)
	LogLevelWarn  LogLevel = LogLevel(slog.LevelWarn)
	LogLevelError LogLevel = LogLevel(slog.LevelError)
)

type Logger interface {
	Debug(msg string, args ...any)
	Info(msg string, args ...any)
	Warn(msg string, args ...any)
	Error(msg string, args ...any)
}

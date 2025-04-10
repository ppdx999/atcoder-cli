package main

import (
	"os"
	"testing"
)

func TestCommandExecute(t *testing.T) {
	var called bool
	root := &Command{
		Usage: "root",
		Run: func(cmd *Command, args []string) ExitCode {
			called = true
			return ExitOK
		},
	}
	os.Args = []string{"root", "sub"}
	root.Execute()
	if !called {
		t.Error("root command was not executed")
	}
}

func TestSubcommandExecute(t *testing.T) {
	var called bool
	root := &Command{
		Usage: "root",
	}
	sub := &Command{
		Usage: "sub",
		Run: func(cmd *Command, args []string) ExitCode {
			called = true
			return ExitOK
		},
	}
	os.Args = []string{"root", "sub"}
	root.AddCommand(sub)
	root.Execute()
	if !called {
		t.Error("subcommand was not executed")
	}
}

func TestErrorExecuteNonRootCmd(t *testing.T) {
	root := &Command{
		Usage: "root",
	}
	sub := &Command{
		Usage: "sub",
	}
	root.AddCommand(sub)
	os.Args = []string{"root", "sub"}
	if sub.Execute() != ExitError {
		t.Error("Execute Subcommand should be error")
	}
}

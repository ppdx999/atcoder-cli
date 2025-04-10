package main

import (
	"os"
	"testing"
)

func TestMain(m *testing.M) {
	// disable stderr
	osStderr := os.Stderr
	defer func() {
		os.Stderr = osStderr
	}()
	os.Stderr = nil

	os.Exit(m.Run())
}

func createCmd(name string, mock func(cmd *Command, args []string) ExitCode) *Command {
	root := &Command{
		Usage: name,
		Run:   mock,
	}
	return root
}

func createMockFn() (func(cmd *Command, args []string) ExitCode, func() bool) {
	isCalled := false

	mockCalled := func() bool {
		return isCalled
	}

	mockFn := func(cmd *Command, args []string) ExitCode {
		isCalled = true
		return ExitOK
	}

	return mockFn, mockCalled
}

func TestCommandExecute(t *testing.T) {
	mockFn, mockCalled := createMockFn()
	root := createCmd("root", mockFn)
	exitCode := root.Execute([]string{})

	if exitCode != ExitOK {
		t.Error("root command finished with error")
	}

	if !mockCalled() {
		t.Error("root command was not executed")
	}
}

func TestSubcommandExecute(t *testing.T) {
	mockFn, mockCalled := createMockFn()
	root := createCmd("root", nil)
	sub := createCmd("sub", mockFn)
	root.AddCommand(sub)

	exitCode := root.Execute([]string{"sub"})

	if exitCode != ExitOK {
		t.Error("root command finished with error")
	}
	if !mockCalled() {
		t.Error("subcommand was not executed")
	}
}

func TestErrorExecuteNonRootCmd(t *testing.T) {
	root := createCmd("root", nil)
	sub := createCmd("sub", nil)
	root.AddCommand(sub)

	if sub.Execute([]string{"sub"}) != ExitError {
		t.Error("Execute Subcommand should be error")
	}
}

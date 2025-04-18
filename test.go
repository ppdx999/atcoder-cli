package main

type TestCmdRunner struct {
	w *Workspace
}

func NewTestCmdRunner(w *Workspace) *TestCmdRunner {
	return &TestCmdRunner{
		w: w,
	}
}

func (c *TestCmdRunner) Setup(cmd *Command[TestCmdOpt]) error {
	c.w = cmd.Workspace()
	return nil
}

func (c *TestCmdRunner) Run(opt TestCmdOpt) ExitCode {
	w := c.w
	w.Debug("Call TestCmdRunner.Run")
	if !c.w.TestCaseExist() {
		c.w.Error("テストケースがダウンロードされていません。")
		return ExitError
	}

	l, err := c.w.DetectLanguage()
	w.Debugf("Detected Language: %s", l.Name)
	if err != nil {
		c.w.Error(err.Error())
		return ExitError
	}

	w.Debug("Building...")
	if err := w.RunOsCmd(l.BuildCmd, nil, c.w.GetOut(), c.w.GetErr()); err != nil {
		c.w.Error(err.Error())
		return ExitError
	}
	w.Debug("Build done")

	w.Debug("Fetching test cases...")
	ts, err := c.w.FetchTestCases()
	if err != nil {
		c.w.Error(err.Error())
		return ExitError
	}
	w.Debug("Fetching test cases done")

	w.Debug("Running and Reporting tests...")
	if err := ts.RunAndReport(l); err != nil {
		c.w.Error(err.Error())
		return ExitError
	}
	w.Debug("Running and Reporting tests done")

	w.Debug("Cleaning up...")
	if err := w.RunOsCmd(l.CleanupCmd, nil, c.w.GetOut(), c.w.GetErr()); err != nil {
		c.w.Error(err.Error())
		return ExitError
	}
	w.Debug("Cleaning up done")

	return ExitOK
}

type TestCmdOpt struct{}

var testCmd = NewCommand(
	&NewCommandOpt[TestCmdOpt]{
		Usage:    "test",
		Short:    "テストコマンド",
		Aliases:  []string{"t"},
		ParseOpt: nil,
		Runner:   &TestCmdRunner{},
	},
)

func init() {
	cmd.Add(testCmd)
}

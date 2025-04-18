package main

import "fmt"

type DownloadCmdRunner struct {
	w       *Workspace
	atcoder *Atcoder
	contest string
	problem string
}

func (c *DownloadCmdRunner) Setup(cmd *Command[DownlaodCmdOpt]) error {
	w := cmd.Workspace()
	contest, problem, err := w.GetContestAndProblem()
	if err != nil {
		return fmt.Errorf("fail to get contest and problem: %v", err)
	}
	c.w = w
	c.atcoder = NewAtcoder(w)
	c.contest = contest
	c.problem = problem
	return nil
}

func (c *DownloadCmdRunner) Run(opt DownlaodCmdOpt) ExitCode {
	ts, err := c.atcoder.GetTestCases(c.contest, c.problem)
	if err != nil {
		c.w.Errorf("Fail to get test cases: %v", err)
		return ExitError
	}
	if err := c.w.MkTestDir(); err != nil {
		c.w.Errorf("Fail to create test dir: %v", err)
		return ExitError
	}
	if err := ts.Write(); err != nil {
		c.w.Errorf("Fail to write test cases: %v", err)
		return ExitError
	}
	return ExitOK
}

type DownlaodCmdOpt struct{}

var downloadCmd = NewCommand(
	&NewCommandOpt[DownlaodCmdOpt]{
		Usage:    "download",
		Short:    "問題のサンプルケースをダウンロードします",
		Aliases:  []string{"d"},
		ParseOpt: nil,
		Runner:   &DownloadCmdRunner{},
	},
)

func init() {
	cmd.Add(downloadCmd)
}

package main

import (
	"fmt"
	"path/filepath"
)

type InitCmdRunner struct {
	w       *Workspace
	atcoder *Atcoder
}

func (c *InitCmdRunner) Setup(cmd *Command[InitCmdOpt]) error {
	w := cmd.Workspace()
	c.w = w
	c.atcoder = NewAtcoder(w)
	return nil
}

func (c *InitCmdRunner) Run(opt InitCmdOpt) ExitCode {
	contest := opt.contest

	pids, err := c.atcoder.GetProblemIDs(contest)
	if err != nil {
		c.w.Error(err.Error())
		return ExitError
	}

	if err := c.w.MkPubDir(contest); err != nil {
		c.w.Error(err.Error())
		return ExitError
	}

	for _, pid := range pids {
		path := filepath.Join(contest, pid)
		if err := c.w.MkPubDir(path); err != nil {
			c.w.Error(err.Error())
			return ExitError
		}
	}

	return ExitOK
}

type InitCmdOpt struct {
	contest string
}

func parseInitCmdOpt(args []string) (InitCmdOpt, error) {
	var opt InitCmdOpt

	if len(args) == 0 {
		return opt, fmt.Errorf("コンテスト名を指定してください")
	}
	opt.contest = args[0]
	return opt, nil
}

var initCmd = NewCommand(
	&NewCommandOpt[InitCmdOpt]{
		Usage:    "init <contest>",
		Short:    "コンテストのディレクトリ構造を初期化します",
		Aliases:  []string{"i"},
		ParseOpt: parseInitCmdOpt,
		Runner:   &InitCmdRunner{},
	},
)

func init() {
	cmd.Add(initCmd)
}

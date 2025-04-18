package main

import (
	"bytes"
	"fmt"
	"path/filepath"
)

type TestCase struct {
	w     *Workspace
	name  string
	input []byte
	want  []byte
	got   []byte
}

func NewTestCase(w *Workspace) *TestCase {
	return &TestCase{w: w}
}

func (t *TestCase) Name() string {
	return t.name
}

func (t *TestCase) SetName(name string) {
	t.name = name
}

func (t *TestCase) Dir() string {
	return t.w.testDir
}

func (t *TestCase) InputFilePath() string {
	return filepath.Join(t.Dir(), t.name+".in")
}

func (t *TestCase) OutputFilePath() string {
	return filepath.Join(t.Dir(), t.name+".out")
}

func (t *TestCase) Input() ([]byte, error) {
	if t.input != nil {
		return t.input, nil
	}
	p := t.InputFilePath()
	if !t.w.ExistFile(p) {
		return nil, fmt.Errorf("input file not found: %s neither input is nil", p)
	}
	input, err := t.w.ReadFile(t.InputFilePath())
	if err != nil {
		return nil, err
	}

	t.input = input
	return input, nil
}

func (t *TestCase) Want() ([]byte, error) {
	if t.want != nil {
		return t.want, nil
	}
	want, err := t.w.ReadFile(t.OutputFilePath())
	if err != nil {
		return nil, err
	}
	t.want = want
	return want, nil
}

func (t *TestCase) Got() ([]byte, error) {
	if t.got != nil {
		return t.got, nil
	}
	return nil, fmt.Errorf("got is nil, please call Run method first")
}

func (t *TestCase) InputWantGot() ([]byte, []byte, []byte, error) {
	input, err := t.Input()
	if err != nil {
		return nil, nil, nil, err
	}
	want, err := t.Want()
	if err != nil {
		return nil, nil, nil, err
	}
	got, err := t.Got()
	if err != nil {
		return nil, nil, nil, err
	}
	return input, want, got, nil
}

func (t *TestCase) SetInput(input []byte) {
	t.input = input
}

func (t *TestCase) SetWant(want []byte) {
	t.want = want
}

func (t *TestCase) Run(l *Language) ([]byte, error) {
	w := t.w
	w.Debugf("Call TestCase.Run: %s", t.Name())
	if t.got != nil {
		return t.got, nil
	}

	input, err := t.Input()
	if err != nil {
		return nil, err
	}

	var buf bytes.Buffer
	if err = w.runOsCmd(l.RunCmd, bytes.NewReader(input), &buf, t.w.GetErr()); err != nil {
		return nil, err
	}
	t.got = bytes.TrimSpace(buf.Bytes())
	w.Debug("Run done: result = %s", string(t.got))

	return t.got, nil
}

func (t *TestCase) IsTestPassed() bool {
	want, err := t.Want()
	if err != nil {
		return false
	}

	got, err := t.Got()
	if err != nil {
		return false
	}

	return bytes.Equal(got, want)
}

func (t *TestCase) Report() error {
	if t.IsTestPassed() {
		t.w.PrintErrlnf("✅ Test %s passed", t.Name())
	} else {
		input, want, got, err := t.InputWantGot()
		if err != nil {
			return err
		}
		t.w.PrintErrlnf("❌ Test %s failed\n", t.Name())
		t.w.PrintErrln("Input:")
		t.w.PrintErrln(string(input))
		t.w.PrintErrln("Expected:")
		t.w.PrintErrln(string(want))
		t.w.PrintErrln("Got:")
		t.w.PrintErrln(string(got))
	}
	return nil
}

func (t *TestCase) write(path string, data []byte) error {
	return t.w.WriteFile(path, data, 0644)
}

func (t *TestCase) Write() error {
	input, err := t.Input()
	if err != nil {
		return err
	}

	if err := t.write(t.InputFilePath(), input); err != nil {
		return err
	}

	want, err := t.Want()
	if err != nil {
		return err
	}

	if err := t.write(t.OutputFilePath(), want); err != nil {
		return err
	}
	return nil
}

type TestCases []*TestCase

func (ts *TestCases) Add(t *TestCase) {
	*ts = append(*ts, t)
}

func (ts *TestCases) Write() error {
	for _, t := range *ts {
		if err := t.Write(); err != nil {
			return err
		}
	}
	return nil
}

func (ts *TestCases) Run(l *Language) error {
	for _, t := range *ts {
		if _, err := t.Run(l); err != nil {
			return err
		}
	}
	return nil
}

func (ts *TestCases) Report() error {
	for _, t := range *ts {
		if err := t.Report(); err != nil {
			return err
		}
	}
	return nil
}

func (ts *TestCases) RunAndReport(l *Language) error {
	if err := ts.Run(l); err != nil {
		return err
	}
	return ts.Report()
}

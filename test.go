package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type language struct {
	Name       string
	MainFile   string
	BuildCmd   []string
	RunCmd     []string
	CleanupCmd []string
}

func (l *language) build(out io.Writer, err io.Writer) error {
	if l.BuildCmd == nil {
		return nil
	}
	cmd := exec.Command(l.BuildCmd[0], l.BuildCmd[1:]...)
	cmd.Stdout = out
	cmd.Stderr = err
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}

func (l *language) run(in io.Reader, out io.Writer, err io.Writer) error {
	cmd := exec.Command(l.RunCmd[0], l.RunCmd[1:]...)
	cmd.Stdin = in
	cmd.Stdout = out
	cmd.Stderr = err
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}

func (l *language) cleanup() error {
	if l.CleanupCmd == nil {
		return nil
	}
	cmd := exec.Command(l.CleanupCmd[0], l.CleanupCmd[1:]...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}

var languages = []language{
	{
		Name:       "go",
		MainFile:   "main.go",
		BuildCmd:   []string{"go", "build", "-o", "main", "main.go"},
		RunCmd:     []string{"./main"},
		CleanupCmd: []string{"rm", "main"},
	},
	{
		Name:       "cpp",
		MainFile:   "main.cpp",
		BuildCmd:   []string{"g++", "-o", "main", "main.cpp"},
		RunCmd:     []string{"./main"},
		CleanupCmd: []string{"rm", "main"},
	},
	{
		Name:     "python",
		MainFile: "main.py",
		RunCmd:   []string{"python3", "main.py"},
	},
	{
		Name:       "zig",
		MainFile:   "main.zig",
		BuildCmd:   []string{"zig", "build", "main.zig"},
		RunCmd:     []string{"./main"},
		CleanupCmd: []string{"rm", "main"},
	},
}

type testCase struct {
	inPath  string
	outPath string
}

func (t *testCase) read(path string) ([]byte, error) {
	buf, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return bytes.TrimSpace(buf), nil
}

func (t *testCase) in() ([]byte, error) {
	return t.read(t.inPath)
}

func (t *testCase) out() ([]byte, error) {
	return t.read(t.outPath)
}

func (t *testCase) run(l *language) ([]byte, error) {
	in, err := t.in()
	if err != nil {
		return nil, err
	}
	var buf bytes.Buffer
	if err = l.run(bytes.NewReader(in), &buf, os.Stderr); err != nil {
		return nil, err
	}
	return bytes.TrimSpace(buf.Bytes()), nil
}

type testCmdRunner struct {
	Command
	languages []language
}

func (c *testCmdRunner) detectLanguage() (*language, error) {
	for _, l := range c.languages {
		if _, err := os.Stat(l.MainFile); err == nil {
			return &l, nil
		}
	}
	return nil, fmt.Errorf("mainファイルが見つかりません。")
}

func (c *testCmdRunner) getTestCases() ([]testCase, error) {
	var cases []testCase
	files, err := os.ReadDir("test")
	if err != nil {
		return nil, err
	}

	for _, f := range files {
		if filepath.Ext(f.Name()) != ".in" {
			continue
		}
		base := strings.TrimSuffix(f.Name(), ".in")
		inPath := filepath.Join("test", base+".in")
		outPath := filepath.Join("test", base+".out")
		cases = append(cases, testCase{
			inPath:  inPath,
			outPath: outPath,
		})
	}
	return cases, nil
}

func (c *testCmdRunner) runTestCases(tcs []testCase, l *language) error {
	for _, tc := range tcs {
		got, err := tc.run(l)
		if err != nil {
			return err
		}

		want, err := tc.out()
		if err != nil {
			return err
		}

		if bytes.Equal(got, want) {
			fmt.Printf("✅ Test %s passed\n", tc.inPath)
		} else {
			fmt.Printf("❌ Test %s failed\n", tc.inPath)
			fmt.Println("Input:")
			in, err := tc.in()
			if err != nil {
				return err
			}
			fmt.Println(string(in))
			fmt.Println("Expected:")
			fmt.Println(string(want))
			fmt.Println("Got:")
			fmt.Println(string(got))
		}
	}
	return nil
}

func (c *testCmdRunner) run() ExitCode {
	lang, err := c.detectLanguage()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}
	if err = lang.build(os.Stdout, os.Stderr); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}

	testCases, err := c.getTestCases()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}

	if err = c.runTestCases(testCases, lang); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}

	if err = lang.cleanup(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}

	return ExitOK
}

func newTestCmdRunner(cmd *Command) *testCmdRunner {
	return &testCmdRunner{
		Command:   *cmd,
		languages: languages,
	}
}

var testCmd = &Command{
	Usage:   "test",
	Short:   "テストコマンド",
	Aliases: []string{"t"},
	Run: func(cmd *Command, args []string) ExitCode {
		return newTestCmdRunner(cmd).run()
	},
}

func init() {
	cmd.AddCommand(testCmd)
}

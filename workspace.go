package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type Workspace struct {
	logger      Logger
	testDir     string
	cookiePath  string
	readFile    func(path string) ([]byte, error)
	writeFile   func(path string, data []byte, perm os.FileMode) error
	getwd       func() (string, error)
	getenv      func(key string) string
	stat        func(path string) (os.FileInfo, error)
	mkdir       func(path string, perm os.FileMode) error
	mkdirAll    func(path string, perm os.FileMode) error
	isExist     func(err error) bool
	isNotExist  func(err error) bool
	readDir     func(path string) ([]os.DirEntry, error)
	userHomeDir func() (string, error)
	runOsCmd    func(cmd []string, stdin io.Reader, stdout, stderr io.Writer) error
	inReader    io.Reader
	outWriter   io.Writer
	errWriter   io.Writer
}

func NewProdWorkspace() *Workspace {
	w := &Workspace{
		logger:      slog.New(slog.NewTextHandler(os.Stderr, nil)),
		testDir:     "test",
		readFile:    os.ReadFile,
		writeFile:   os.WriteFile,
		getwd:       os.Getwd,
		getenv:      os.Getenv,
		stat:        os.Stat,
		mkdir:       os.Mkdir,
		mkdirAll:    os.MkdirAll,
		isExist:     os.IsExist,
		isNotExist:  os.IsNotExist,
		readDir:     os.ReadDir,
		userHomeDir: os.UserHomeDir,
		runOsCmd: func(cmd []string, stdin io.Reader, stdout, stderr io.Writer) error {
			c := exec.Command(cmd[0], cmd[1:]...)
			c.Stdin = stdin
			c.Stdout = stdout
			c.Stderr = stderr
			return c.Run()
		},
	}
	w.cookiePath = prodCookiePath(w)
	return w
}

func (w *Workspace) GetIn() io.Reader {
	if w.inReader == nil {
		return os.Stdin
	}
	return w.inReader
}

func (w *Workspace) SetIn(r io.Reader) {
	w.inReader = r
}

func (w *Workspace) GetOut() io.Writer {
	if w.outWriter == nil {
		return os.Stdout
	}
	return w.outWriter
}

func (w *Workspace) SetOut(wr io.Writer) {
	w.outWriter = wr
}

func (w *Workspace) GetErr() io.Writer {
	if w.errWriter == nil {
		return os.Stderr
	}
	return w.errWriter
}

func (w *Workspace) SetErr(wr io.Writer) {
	w.errWriter = wr
}

func (w *Workspace) GetLogger() Logger {
	if w.logger == nil {
		return slog.New(slog.NewTextHandler(os.Stderr, nil))
	}
	return w.logger
}

func (w *Workspace) SetLogger(logger Logger) {
	w.logger = logger
}

func (w *Workspace) SetLogLevel(level LogLevel) {
	w.logger = slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: slog.Level(level)}))
}

func (w *Workspace) PrintErrln(a ...any) {
	fmt.Fprintln(w.GetErr(), a...)
}

func (w *Workspace) PrintErrlnf(format string, a ...any) {
	w.PrintErrln(fmt.Sprintf(format, a...))
}

func (w *Workspace) Info(msg string, args ...any) {
	w.GetLogger().Info(msg, args...)
}

func (w *Workspace) Infof(format string, a ...any) {
	w.Info(fmt.Sprintf(format, a...))
}

func (w *Workspace) Debug(msg string, args ...any) {
	w.GetLogger().Debug(msg, args...)
}

func (w *Workspace) Debugf(format string, a ...any) {
	w.Debug(fmt.Sprintf(format, a...))
}

func (w *Workspace) Error(msg string, args ...any) {
	w.GetLogger().Error(msg, args...)
}

func (w *Workspace) Errorf(format string, a ...any) {
	w.Error(fmt.Sprintf(format, a...))
}

func (w *Workspace) Warn(msg string, args ...any) {
	w.GetLogger().Warn(msg, args...)
}

func (w *Workspace) Warnf(format string, a ...any) {
	w.Warn(fmt.Sprintf(format, a...))
}

func (w *Workspace) Mkdir(path string, perm os.FileMode) error {
	if err := w.mkdir(path, perm); err != nil && !w.isExist(err) {
		return err
	}
	return nil
}

func (w *Workspace) MkPubDir(path string) error {
	return w.Mkdir(path, 0755)
}

func (w *Workspace) MkPubDirAll(path string) error {
	return w.mkdirAll(path, 0755)
}

func (w *Workspace) ReadFile(path string) ([]byte, error) {
	return w.readFile(path)
}

func (w *Workspace) WriteFile(path string, data []byte, perm os.FileMode) error {
	return w.writeFile(path, data, perm)
}

func (w *Workspace) ExistFile(path string) bool {
	if _, err := w.stat(path); w.isNotExist(err) {
		return false
	}
	return true
}

func (w *Workspace) GetContestAndProblem() (contest string, problem string, err error) {
	cwd, err := w.getwd()
	if err != nil {
		return "", "", err
	}
	if problem = filepath.Base(cwd); problem == "/" {
		return "", "", errors.New("no contest or problem found")
	}

	if contest = filepath.Base(filepath.Dir(cwd)); contest == "/" {
		return "", "", errors.New("no contest or problem found")
	}

	return contest, problem, nil
}

func (w *Workspace) DetectLanguage() (*Language, error) {
	for _, l := range Languages {
		if _, err := w.stat(l.MainFile); err == nil {
			return &l, nil
		}
	}
	return nil, fmt.Errorf("mainファイルが見つかりません。")
}

func (w *Workspace) TestCaseExist() bool {
	if !w.ExistFile(w.testDir) {
		return false
	}

	files, err := w.readDir(w.testDir)
	if err != nil {
		w.PrintErrln(err)
		return false
	}
	for _, f := range files {
		if filepath.Ext(f.Name()) == ".in" {
			return true
		}
	}
	return false
}

func (w *Workspace) MkTestDir() error {
	return w.MkPubDir(w.testDir)
}

func (w *Workspace) FetchTestCases() (TestCases, error) {
	var ts TestCases
	files, err := w.readDir(w.testDir)
	if err != nil {
		return nil, err
	}

	for _, f := range files {
		if filepath.Ext(f.Name()) != ".in" {
			continue
		}
		name := strings.TrimSuffix(f.Name(), ".in")
		t := NewTestCase(w)
		t.SetName(name)
		ts.Add(t)
	}
	return ts, nil
}

func (w *Workspace) LoadCookies() ([]*http.Cookie, error) {
	if !w.ExistFile(w.cookiePath) {
		return nil, nil
	}

	data, err := w.ReadFile(w.cookiePath)
	if err != nil {
		return nil, err
	}

	var cookies []*http.Cookie
	lines := strings.SplitSeq(string(data), "\n")
	for line := range lines {
		if line == "" {
			continue
		}
		parts := strings.SplitN(line, "=", 2)
		if len(parts) != 2 {
			continue
		}
		if parts[0] == "" {
			continue
		}
		cookies = append(cookies, &http.Cookie{
			Name:  parts[0],
			Value: parts[1],
		})
	}
	return cookies, nil
}

func (w *Workspace) SaveCookie(cookie string) error {
	if err := w.MkPubDirAll(filepath.Dir(w.cookiePath)); err != nil {
		return err
	}
	return w.WriteFile(w.cookiePath, []byte(cookie+"\n"), 0600)
}

func (w *Workspace) PromptCookie() (string, error) {
	w.PrintErrln("REVEL_SESSION cookie を入力してください: ")

	reader := bufio.NewReader(w.GetIn())
	cookie, err := reader.ReadString('\n')
	if err != nil {
		return "", err
	}
	cookie = strings.TrimSpace(cookie)

	return "REVEL_SESSION=" + cookie, nil
}

func prodCookiePath(w *Workspace) string {
	xdg := w.getenv("XDG_DATA_HOME")
	if xdg == "" {
		home, _ := w.userHomeDir()
		xdg = filepath.Join(home, ".local", "share")
	}
	return filepath.Join(xdg, "atcoder-cli", "cookie.txt")
}

func (w *Workspace) RunOsCmd(cmd []string, stdin io.Reader, stdout, stderr io.Writer) error {
	return w.runOsCmd(cmd, stdin, stdout, stderr)
}

var ProdWorkspace = NewProdWorkspace()

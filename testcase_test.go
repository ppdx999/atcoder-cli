package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestTestCase_Name(t *testing.T) {
	w := newTestWorkspace()
	tc := NewTestCase(w)
	tc.SetName("sample-1")
	if got := tc.Name(); got != "sample-1" {
		t.Errorf("Name() = %q, want %q", got, "sample-1")
	}
}

func TestTestCase_Paths(t *testing.T) {
	w := newTestWorkspace()
	w.testDir = "my_test_dir"
	tc := NewTestCase(w)
	tc.SetName("case1")

	if got := tc.Dir(); got != "my_test_dir" {
		t.Errorf("Dir() = %q, want %q", got, "my_test_dir")
	}
	wantInputPath := filepath.Join("my_test_dir", "case1.in")
	if got := tc.InputFilePath(); got != wantInputPath {
		t.Errorf("InputFilePath() = %q, want %q", got, wantInputPath)
	}
	wantOutputPath := filepath.Join("my_test_dir", "case1.out")
	if got := tc.OutputFilePath(); got != wantOutputPath {
		t.Errorf("OutputFilePath() = %q, want %q", got, wantOutputPath)
	}
}

func TestTestCase_Input(t *testing.T) {
	tests := []struct {
		name            string
		initialInput    []byte // 事前に SetInput しておくデータ
		mockReadFile    []byte
		mockReadFileErr error
		mockExistFile   bool
		wantInput       []byte
		wantErr         bool
		wantErrString   string
	}{
		{
			name:         "キャッシュあり",
			initialInput: []byte("cached input"),
			wantInput:    []byte("cached input"),
			wantErr:      false,
		},
		{
			name:          "キャッシュなし、ファイル読み込み成功",
			mockReadFile:  []byte("file input"),
			mockExistFile: true,
			wantInput:     []byte("file input"),
			wantErr:       false,
		},
		{
			name:          "キャッシュなし、ファイル存在しない",
			mockExistFile: false,
			wantErr:       true,
			wantErrString: "input file not found",
		},
		{
			name:            "キャッシュなし、ファイル読み込みエラー",
			mockReadFileErr: errors.New("read error"),
			mockExistFile:   true,
			wantErr:         true,
			wantErrString:   "read error",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			tc := NewTestCase(w)
			tc.SetName("test")
			inputPath := tc.InputFilePath()

			if tt.initialInput != nil {
				tc.SetInput(tt.initialInput)
			}

			w.readFile = func(path string) ([]byte, error) {
				if path == inputPath {
					return tt.mockReadFile, tt.mockReadFileErr
				}
				return nil, fmt.Errorf("unexpected readFile call for %q", path)
			}
			// ExistFile は stat と isNotExist を使うので、stat をモック化
			w.stat = func(path string) (os.FileInfo, error) {
				if path == inputPath {
					if tt.mockExistFile {
						return &mockFileInfo{}, nil // 存在する
					}
					return nil, os.ErrNotExist // 存在しない
				}
				return nil, fmt.Errorf("unexpected stat call for %q", path)
			}

			gotInput, err := tc.Input()

			if (err != nil) != tt.wantErr {
				t.Errorf("Input() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr && tt.wantErrString != "" && !strings.Contains(err.Error(), tt.wantErrString) {
				t.Errorf("Input() error = %q, wantErr containing %q", err.Error(), tt.wantErrString)
			}
			if !tt.wantErr && !bytes.Equal(gotInput, tt.wantInput) {
				t.Errorf("Input() got = %q, want %q", string(gotInput), string(tt.wantInput))
			}
			// 読み込み成功後、再度呼び出してキャッシュが使われるか確認
			if !tt.wantErr && tt.initialInput == nil {
				readFileCalledAgain := false
				w.readFile = func(path string) ([]byte, error) {
					readFileCalledAgain = true
					t.Error("readFile should not be called again (cache miss)")
					return nil, errors.New("cache miss")
				}
				gotInput2, err2 := tc.Input()
				if err2 != nil {
					t.Errorf("Second Input() call failed: %v", err2)
				}
				if !bytes.Equal(gotInput2, tt.wantInput) {
					t.Errorf("Second Input() got = %q, want %q", string(gotInput2), string(tt.wantInput))
				}
				if readFileCalledAgain {
					t.Error("readFile was called on second Input() call")
				}
			}
		})
	}
}

func TestTestCase_Want(t *testing.T) {
	// Input とほぼ同じロジックなので、代表的なケースのみテスト
	tests := []struct {
		name            string
		initialWant     []byte // 事前に SetWant しておくデータ
		mockReadFile    []byte
		mockReadFileErr error
		wantWant        []byte
		wantErr         bool
		wantErrString   string
	}{
		{
			name:        "キャッシュあり",
			initialWant: []byte("cached want"),
			wantWant:    []byte("cached want"),
			wantErr:     false,
		},
		{
			name:         "キャッシュなし、ファイル読み込み成功",
			mockReadFile: []byte("file want"),
			wantWant:     []byte("file want"),
			wantErr:      false,
		},
		{
			name:            "キャッシュなし、ファイル読み込みエラー",
			mockReadFileErr: errors.New("read error"),
			wantErr:         true,
			wantErrString:   "read error",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			tc := NewTestCase(w)
			tc.SetName("test")
			outputPath := tc.OutputFilePath()

			if tt.initialWant != nil {
				tc.SetWant(tt.initialWant)
			}

			w.readFile = func(path string) ([]byte, error) {
				if path == outputPath {
					return tt.mockReadFile, tt.mockReadFileErr
				}
				return nil, fmt.Errorf("unexpected readFile call for %q", path)
			}

			gotWant, err := tc.Want()

			if (err != nil) != tt.wantErr {
				t.Errorf("Want() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr && tt.wantErrString != "" && !strings.Contains(err.Error(), tt.wantErrString) {
				t.Errorf("Want() error = %q, wantErr containing %q", err.Error(), tt.wantErrString)
			}
			if !tt.wantErr && !bytes.Equal(gotWant, tt.wantWant) {
				t.Errorf("Want() got = %q, want %q", string(gotWant), string(tt.wantWant))
			}
			// 読み込み成功後、再度呼び出してキャッシュが使われるか確認
			if !tt.wantErr && tt.initialWant == nil {
				readFileCalledAgain := false
				w.readFile = func(path string) ([]byte, error) {
					readFileCalledAgain = true
					t.Error("readFile should not be called again (cache miss)")
					return nil, errors.New("cache miss")
				}
				gotWant2, err2 := tc.Want()
				if err2 != nil {
					t.Errorf("Second Want() call failed: %v", err2)
				}
				if !bytes.Equal(gotWant2, tt.wantWant) {
					t.Errorf("Second Want() got = %q, want %q", string(gotWant2), string(tt.wantWant))
				}
				if readFileCalledAgain {
					t.Error("readFile was called on second Want() call")
				}
			}
		})
	}
}

func TestTestCase_Got(t *testing.T) {
	w := newTestWorkspace()
	tc := NewTestCase(w)

	// 初期状態ではエラー
	_, err := tc.Got()
	if err == nil {
		t.Errorf("Got() should return error initially, but got nil")
	} else {
		wantErrMsg := "got is nil"
		if !strings.Contains(err.Error(), wantErrMsg) {
			t.Errorf("Got() error = %q, want containing %q", err.Error(), wantErrMsg)
		}
	}

	// Run で設定された後
	tc.got = []byte("run result")
	got, err := tc.Got()
	if err != nil {
		t.Errorf("Got() returned error after setting: %v", err)
	}
	if !bytes.Equal(got, []byte("run result")) {
		t.Errorf("Got() = %q, want %q", string(got), "run result")
	}
}

func TestTestCase_InputWantGot(t *testing.T) {
	w := newTestWorkspace()
	tc := NewTestCase(w)
	tc.SetName("test")

	// 正常系
	tc.SetInput([]byte("in"))
	tc.SetWant([]byte("want"))
	tc.got = []byte("got") // Got() がエラーを返さないように設定

	in, want, got, err := tc.InputWantGot()
	if err != nil {
		t.Errorf("InputWantGot() returned error unexpectedly: %v", err)
	}
	if !bytes.Equal(in, []byte("in")) {
		t.Errorf("Input mismatch")
	}
	if !bytes.Equal(want, []byte("want")) {
		t.Errorf("Want mismatch")
	}
	if !bytes.Equal(got, []byte("got")) {
		t.Errorf("Got mismatch")
	}

	// Input でエラー
	w.readFile = func(path string) ([]byte, error) { return nil, errors.New("input error") }
	w.stat = func(path string) (os.FileInfo, error) { return &mockFileInfo{}, nil }
	tc.input = nil // キャッシュをクリア
	_, _, _, err = tc.InputWantGot()
	if err == nil || !strings.Contains(err.Error(), "input error") {
		t.Errorf("InputWantGot() expected input error, got %v", err)
	}
	tc.SetInput([]byte("in")) // エラー状態をリセット

	// Want でエラー
	w.readFile = func(path string) ([]byte, error) {
		if strings.HasSuffix(path, ".in") {
			return []byte("in"), nil
		}
		return nil, errors.New("want error")
	}
	tc.want = nil // キャッシュをクリア
	_, _, _, err = tc.InputWantGot()
	if err == nil || !strings.Contains(err.Error(), "want error") {
		t.Errorf("InputWantGot() expected want error, got %v", err)
	}
	tc.SetWant([]byte("want")) // エラー状態をリセット

	// Got でエラー
	tc.got = nil // Got() がエラーを返すように設定
	_, _, _, err = tc.InputWantGot()
	if err == nil || !strings.Contains(err.Error(), "got is nil") {
		t.Errorf("InputWantGot() expected got error, got %v", err)
	}
}

func TestTestCase_Run(t *testing.T) {
	tests := []struct {
		name          string
		input         []byte                                                               // tc.SetInput
		inputErr      error                                                                // tc.Input() が返すエラー
		mockRunOsCmd  func(cmd []string, in io.Reader, out io.Writer, err io.Writer) error // w.runOsCmd のモック
		wantGot       []byte
		wantErr       bool
		wantErrString string
		wantStderr    string
		runAgainCheck bool // 2回目の Run で mockRunFunc が呼ばれないか
	}{
		{
			name:  "正常系",
			input: []byte("hello"),
			mockRunOsCmd: func(cmd []string, in io.Reader, out io.Writer, errW io.Writer) error {
				data, _ := io.ReadAll(in)
				if string(data) != "hello" {
					return fmt.Errorf("unexpected input to runOsCmd: %q", string(data))
				}
				fmt.Fprint(out, "world\n ") // 末尾の空白は TrimSpace されるはず
				fmt.Fprint(errW, "debug message")
				return nil
			},
			wantGot:       []byte("world"),
			wantErr:       false,
			wantStderr:    "debug message",
			runAgainCheck: true,
		},
		{
			name:          "Input でエラー",
			inputErr:      errors.New("input error"),
			wantErr:       true,
			wantErrString: "input error",
		},
		{
			name:  "Language.Run でエラー",
			input: []byte("hello"),
			mockRunOsCmd: func(cmd []string, in io.Reader, out io.Writer, errW io.Writer) error {
				// エラーをシミュレート
				fmt.Fprint(errW, "actual runtime error") // エラー出力もシミュレート
				return errors.New("run failed")
			},
			wantErr:       true,
			wantErrString: "run failed",
			wantStderr:    "actual runtime error",
		},
		{
			name:  "got が既に設定されている",
			input: []byte("hello"),
			mockRunOsCmd: func(cmd []string, in io.Reader, out io.Writer, errW io.Writer) error {
				// この関数は呼ばれないはず
				t.Error("runOsCmd should not be called when got is already set")
				return errors.New("should not be called")
			},
			wantGot: []byte("already set"), // 事前に設定しておく値
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			tc := NewTestCase(w)
			tc.SetName("test")
			errBuf := &bytes.Buffer{} // Workspace の標準エラーをキャプチャ
			w.SetErr(errBuf)

			// Input のモック化
			if tt.inputErr != nil {
				// Input() がエラーを返すように readFile を設定
				w.readFile = func(path string) ([]byte, error) {
					if path == tc.InputFilePath() {
						return nil, tt.inputErr
					}
					return nil, fmt.Errorf("unexpected readFile call for %q", path)
				}
				w.stat = func(path string) (os.FileInfo, error) {
					if path == tc.InputFilePath() {
						return &mockFileInfo{}, nil // ファイルは存在すると仮定
					}
					return nil, fmt.Errorf("unexpected stat call for %q", path)
				}
			} else {
				tc.SetInput(tt.input)
			}

			// Workspace の runOsCmd をモック化
			runOsCmdCalled := false
			w.runOsCmd = func(cmd []string, stdin io.Reader, stdout, stderr io.Writer) error {
				runOsCmdCalled = true
				if tt.mockRunOsCmd != nil {
					return tt.mockRunOsCmd(cmd, stdin, stdout, stderr)
				}
				// モック関数が設定されていない場合はエラー
				return errors.New("mockRunOsCmd not set for this test case")
			}

			// got が既に設定されている場合のテスト
			if tt.name == "got が既に設定されている" {
				tc.got = tt.wantGot
			}

			// tc.Run に渡す Language は RunCmd を提供するためだけに使用される
			lang := NewMockLang() // RunCmd を持つダミーの Language

			got, err := tc.Run(lang)

			if (err != nil) != tt.wantErr {
				t.Errorf("Run() error = %v, wantErr %v", err, tt.wantErr)
				// エラー発生時は以降のチェックはスキップ
				return
			}
			if tt.wantErr {
				if tt.wantErrString != "" && !strings.Contains(err.Error(), tt.wantErrString) {
					t.Errorf("Run() error = %q, wantErr containing %q", err.Error(), tt.wantErrString)
				}
				// エラー発生時でも runOsCmd が stderr に書き込んだ内容を確認
				if gotStderr := errBuf.String(); !strings.Contains(gotStderr, tt.wantStderr) {
					t.Errorf("Run() stderr = %q, want containing %q", gotStderr, tt.wantStderr)
				}
				return // 正常系のチェックはスキップ
			}

			// 正常系のチェック
			if !bytes.Equal(got, tt.wantGot) {
				t.Errorf("Run() got = %q, want %q", string(got), string(tt.wantGot))
			}
			// runOsCmd が stderr に書き込んだ内容を確認
			if gotStderr := errBuf.String(); !strings.Contains(gotStderr, tt.wantStderr) {
				t.Errorf("Run() stderr = %q, want containing %q", gotStderr, tt.wantStderr)
			}

			// 2回目の Run 呼び出しでキャッシュが効くか (runOsCmd が呼ばれないか)
			if tt.runAgainCheck {
				runOsCmdCalledAgain := false
				// モック関数を差し替えて、呼ばれたらエラーにする
				w.runOsCmd = func(cmd []string, stdin io.Reader, stdout, stderr io.Writer) error {
					runOsCmdCalledAgain = true
					t.Error("runOsCmd should not be called again (cache miss)")
					return errors.New("cache miss")
				}

				got2, err2 := tc.Run(lang) // 再度実行

				if err2 != nil {
					t.Errorf("Second Run() call failed: %v", err2)
				}
				if !bytes.Equal(got2, tt.wantGot) {
					t.Errorf("Second Run() got = %q, want %q", string(got2), string(tt.wantGot))
				}
				if runOsCmdCalledAgain {
					t.Error("runOsCmd was called on second Run() call")
				}
				if !runOsCmdCalled && tt.inputErr == nil { // 初回呼び出しで runOsCmd が呼ばれたかも確認
					t.Error("runOsCmd was not called on the first successful Run()")
				}
			} else if tt.name != "got が既に設定されている" && tt.inputErr == nil {
				// runAgainCheck=false だが、初回で呼ばれるべきケース
				if !runOsCmdCalled {
					t.Error("runOsCmd was not called when expected")
				}
			} else if tt.name == "got が既に設定されている" {
				// got が設定済みのケースでは呼ばれないことを確認
				if runOsCmdCalled {
					t.Error("runOsCmd was called when got was already set")
				}
			}
		})
	}
}

func TestTestCase_IsTestPassed(t *testing.T) {
	tests := []struct {
		name    string
		want    []byte
		wantErr error  // Want() が返すエラー
		got     []byte // tc.got に設定する値
		gotErr  bool   // Got() がエラーを返すか (tc.got が nil かどうか)
		wantRes bool
	}{
		{
			name:    "成功",
			want:    []byte("output"),
			got:     []byte("output"),
			wantRes: true,
		},
		{
			name:    "失敗",
			want:    []byte("expected"),
			got:     []byte("actual"),
			wantRes: false,
		},
		{
			name:    "Want でエラー",
			wantErr: errors.New("want error"),
			got:     []byte("actual"),
			wantRes: false,
		},
		{
			name:    "Got でエラー",
			want:    []byte("expected"),
			gotErr:  true, // tc.got を nil のままにする
			wantRes: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			tc := NewTestCase(w)
			tc.SetName("test")
			outputPath := tc.OutputFilePath()

			// Want のモック化
			if tt.wantErr != nil {
				w.readFile = func(path string) ([]byte, error) {
					if path == outputPath {
						return nil, tt.wantErr
					}
					return nil, fmt.Errorf("unexpected readFile call for %q", path)
				}
			} else {
				tc.SetWant(tt.want)
			}

			// Got の設定
			if !tt.gotErr {
				tc.got = tt.got // Got() がエラーを返さないように設定
			}
			// Got() がエラーを返すケースは、tc.got が nil のままにしておく

			if gotRes := tc.IsTestPassed(); gotRes != tt.wantRes {
				t.Errorf("IsTestPassed() = %v, want %v", gotRes, tt.wantRes)
			}
		})
	}
}

func TestTestCase_Report(t *testing.T) {
	tests := []struct {
		name       string
		isPassed   bool // IsTestPassed が返す値のシミュレーション
		input      []byte
		want       []byte
		got        []byte
		inputErr   error  // Input() が返すエラー
		wantFnErr  error  // Want() が返すエラー
		gotErr     bool   // Got() がエラーを返すか
		wantOutput string // 標準エラーへの期待出力 (部分一致)
		wantErr    bool   // Report() が返すエラー
	}{
		{
			name:       "成功",
			isPassed:   true,
			wantOutput: "✅ Test test passed",
			wantErr:    false,
		},
		{
			name:       "失敗",
			isPassed:   false,
			input:      []byte("in"),
			want:       []byte("exp"),
			got:        []byte("act"),
			wantOutput: "❌ Test test failed", // 他の出力も含まれるが、まずこれを確認
			wantErr:    false,
		},
		{
			name:       "失敗 (InputWantGot で Input エラー)",
			isPassed:   false,
			inputErr:   errors.New("input error"), // InputWantGot がエラーを返すように
			wantOutput: "",                        // エラーが返るので出力はないはず
			wantErr:    true,
		},
		{
			name:       "失敗 (InputWantGot で Want エラー)",
			isPassed:   false,
			input:      []byte("in"),
			wantFnErr:  errors.New("want error"), // InputWantGot がエラーを返すように
			wantOutput: "",
			wantErr:    true,
		},
		{
			name:       "失敗 (InputWantGot で Got エラー)",
			isPassed:   false,
			input:      []byte("in"),
			want:       []byte("exp"),
			gotErr:     true, // InputWantGot がエラーを返すように
			wantOutput: "",
			wantErr:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			tc := NewTestCase(w)
			tc.SetName("test")
			errBuf := &bytes.Buffer{}
			w.SetErr(errBuf)
			inputPath := tc.InputFilePath()
			outputPath := tc.OutputFilePath()

			// IsTestPassed, Input, Want, Got のモック化/設定
			if tt.isPassed {
				// IsTestPassed が true を返すように Want と Got を一致させる
				tc.SetWant([]byte("pass"))
				tc.got = []byte("pass")
			} else {
				// IsTestPassed が false を返すように Want と Got を不一致にするか、エラーを設定
				if tt.wantFnErr != nil {
					w.readFile = func(path string) ([]byte, error) {
						if path == inputPath {
							return tt.input, nil
						} // Input は成功させる
						if path == outputPath {
							return nil, tt.wantFnErr
						}
						return nil, fmt.Errorf("unexpected readFile call for %q", path)
					}
					w.stat = func(path string) (os.FileInfo, error) { return &mockFileInfo{}, nil }
					tc.SetInput(tt.input) // Input はキャッシュしておく
					tc.got = tt.got       // Got はエラーなし
				} else if tt.inputErr != nil {
					w.readFile = func(path string) ([]byte, error) { return nil, tt.inputErr }
					w.stat = func(path string) (os.FileInfo, error) { return &mockFileInfo{}, nil }
					tc.SetWant(tt.want) // Want はエラーなし
					tc.got = tt.got     // Got はエラーなし
				} else if tt.gotErr {
					tc.SetInput(tt.input)
					tc.SetWant(tt.want)
					tc.got = nil // Got() がエラーを返すように
				} else {
					// 通常の失敗ケース
					tc.SetInput(tt.input)
					tc.SetWant(tt.want)
					tc.got = tt.got
				}
			}

			err := tc.Report()

			if (err != nil) != tt.wantErr {
				t.Errorf("Report() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if !tt.wantErr {
				gotOutput := errBuf.String()
				if !strings.Contains(gotOutput, tt.wantOutput) {
					t.Errorf("Report() output = %q, want containing %q", gotOutput, tt.wantOutput)
				}
				// 失敗時の詳細出力も確認 (エラーがない場合)
				if !tt.isPassed && tt.inputErr == nil && tt.wantFnErr == nil && !tt.gotErr {
					if !strings.Contains(gotOutput, "Input:\nin") {
						t.Errorf("Report() output missing input detail: %q", gotOutput)
					}
					if !strings.Contains(gotOutput, "Expected:\nexp") {
						t.Errorf("Report() output missing want detail: %q", gotOutput)
					}
					if !strings.Contains(gotOutput, "Got:\nact") {
						t.Errorf("Report() output missing got detail: %q", gotOutput)
					}
				}
			}
		})
	}
}

func TestTestCase_Write(t *testing.T) {
	tests := []struct {
		name             string
		input            []byte
		want             []byte
		inputErr         error             // Input() が返すエラー
		wantErrErr       error             // Want() が返すエラー
		writeFileErr     error             // w.writeFile が返すエラー
		writeFileErrPath string            // writeFileErr を発生させるパス (".in" or ".out")
		wantWriteCalls   map[string][]byte // path -> data
		wantErr          bool
		wantErrString    string
	}{
		{
			name:  "正常系",
			input: []byte("in data"),
			want:  []byte("out data"),
			wantWriteCalls: map[string][]byte{
				"test/test.in":  []byte("in data"),
				"test/test.out": []byte("out data"),
			},
			wantErr: false,
		},
		{
			name:           "Input でエラー",
			inputErr:       errors.New("input error"),
			wantWriteCalls: map[string][]byte{},
			wantErr:        true,
			wantErrString:  "input error",
		},
		{
			name:       "Want でエラー",
			input:      []byte("in data"),
			wantErrErr: errors.New("want error"),
			wantWriteCalls: map[string][]byte{ // Input は書き込まれるはず
				"test/test.in": []byte("in data"),
			},
			wantErr:       true,
			wantErrString: "want error",
		},
		{
			name:             "WriteFile でエラー (Input)",
			input:            []byte("in data"),
			want:             []byte("out data"),
			writeFileErr:     errors.New("write error"),
			writeFileErrPath: ".in",
			wantWriteCalls: map[string][]byte{ // Input 書き込み試行
				"test/test.in": []byte("in data"),
			},
			wantErr:       true,
			wantErrString: "write error",
		},
		{
			name:             "WriteFile でエラー (Output)",
			input:            []byte("in data"),
			want:             []byte("out data"),
			writeFileErr:     errors.New("write error on output"), // Output 書き込み時にエラー
			writeFileErrPath: ".out",
			wantWriteCalls: map[string][]byte{
				"test/test.in":  []byte("in data"),
				"test/test.out": []byte("out data"), // Output 書き込み試行
			},
			wantErr:       true,
			wantErrString: "write error on output",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			tc := NewTestCase(w)
			tc.SetName("test")
			inputPath := tc.InputFilePath()
			outputPath := tc.OutputFilePath()

			// Input, Want のモック化/設定
			if tt.inputErr != nil {
				w.readFile = func(path string) ([]byte, error) { return nil, tt.inputErr }
				w.stat = func(path string) (os.FileInfo, error) { return &mockFileInfo{}, nil }
			} else {
				tc.SetInput(tt.input)
			}
			if tt.wantErrErr != nil {
				// Want() がエラーを返すように readFile を設定
				// Input() が成功する必要があるので、inputPath の読み込みは成功させる
				w.readFile = func(path string) ([]byte, error) {
					if path == inputPath {
						return tt.input, nil
					}
					if path == outputPath {
						return nil, tt.wantErrErr
					}
					return nil, fmt.Errorf("unexpected readFile call for %q", path)
				}
				w.stat = func(path string) (os.FileInfo, error) { return &mockFileInfo{}, nil }
			} else {
				tc.SetWant(tt.want)
			}

			gotWriteCalls := make(map[string][]byte)
			w.writeFile = func(path string, data []byte, perm os.FileMode) error {
				gotWriteCalls[path] = append([]byte{}, data...) // スライスをコピーして保存
				if perm != 0644 {
					t.Errorf("writeFile called with perm %v, want 0644", perm)
				}
				// エラーをシミュレート
				if tt.writeFileErr != nil {
					if (tt.writeFileErrPath == ".in" && path == inputPath) ||
						(tt.writeFileErrPath == ".out" && path == outputPath) {
						return tt.writeFileErr
					}
				}
				return nil // 正常終了
			}

			err := tc.Write()

			if (err != nil) != tt.wantErr {
				t.Errorf("Write() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr && tt.wantErrString != "" && !strings.Contains(err.Error(), tt.wantErrString) {
				t.Errorf("Write() error = %q, wantErr containing %q", err.Error(), tt.wantErrString)
			}

			// writeFile の呼び出し内容を検証
			if !reflect.DeepEqual(gotWriteCalls, tt.wantWriteCalls) {
				t.Errorf("Write() write calls mismatch")
				// 詳細表示
				t.Logf("Got calls:")
				for k, v := range gotWriteCalls {
					t.Logf("  path=%q, data=%q", k, string(v))
				}
				t.Logf("Want calls:")
				for k, v := range tt.wantWriteCalls {
					t.Logf("  path=%q, data=%q", k, string(v))
				}
			}
		})
	}
}

// --- TestCases テスト ---

// mockTestCase は TestCase のメソッド呼び出しを記録するモックインターフェース
type mockTestCaseInterface interface {
	Name() string
	Write() error
	Run(l *Language) error
	Report() error
}

// mockTestCaseImpl は mockTestCaseInterface の実装
type mockTestCaseImpl struct {
	name         string
	writeCalled  bool
	writeErr     error
	runCalled    bool
	runErr       error
	reportCalled bool
	reportErr    error
}

func (m *mockTestCaseImpl) Name() string { return m.name }
func (m *mockTestCaseImpl) Write() error {
	m.writeCalled = true
	return m.writeErr
}
func (m *mockTestCaseImpl) Run(l *Language) error {
	m.runCalled = true
	return m.runErr
}
func (m *mockTestCaseImpl) Report() error {
	m.reportCalled = true
	return m.reportErr
}

// newMockTestCase は新しいモックテストケースを作成します
func newMockTestCase(name string) *mockTestCaseImpl {
	return &mockTestCaseImpl{name: name}
}

func TestTestCases_Add(t *testing.T) {
	var ts TestCases
	tc1 := &TestCase{name: "1"} // 実際の TestCase を使う
	tc2 := &TestCase{name: "2"}

	ts.Add(tc1)
	if len(ts) != 1 || ts[0] != tc1 {
		t.Errorf("Add failed after first add")
	}

	ts.Add(tc2)
	if len(ts) != 2 || ts[0] != tc1 || ts[1] != tc2 {
		t.Errorf("Add failed after second add")
	}
}

// TestCases のメソッドテスト用に、TestCase のスライスを mockTestCaseInterface のスライスに変換するヘルパー
func toMockTestCases(mocks []*mockTestCaseImpl) TestCases {
	// TestCases は []*TestCase 型なので、モックを直接入れることはできない。
	// 代わりに、実際の TestCase を作成し、その Write, Run, Report メソッド内で
	// 対応するモックのメソッドを呼び出すようにする。
	// ここでは簡単化のため、TestCases のテストではモックを使わず、
	// 実際の TestCase を使って、その内部状態（Write/Run/Reportが呼ばれたか）を
	// 間接的に確認するアプローチをとる。
	// （より厳密には、TestCaseInterface を定義して TestCases がそれを受け入れるように
	//   リファクタリングするのが良いが、既存コードの変更を避ける）

	// --- 代替アプローチ ---
	// TestCases のテストでは、実際の TestCase を使い、
	// Workspace のモックを使って Write/Run/Report の呼び出しを確認する。

	// このヘルパー関数は使わないのでコメントアウト
	/*
		var cases TestCases
		for _, m := range mocks {
			// ダミーの TestCase を作成し、モックのメソッドを呼び出すようにする
			// (この方法は複雑になるため、ここでは採用しない)
		}
		return cases
	*/
	return nil // この関数は使わない
}

func TestTestCases_Write(t *testing.T) {
	w := newTestWorkspace()
	writeCalls := make(map[string]bool) //どのファイルの書き込みが呼ばれたか
	w.writeFile = func(path string, data []byte, perm os.FileMode) error {
		writeCalls[path] = true
		if path == "test/2.in" { // tc2 の Write でエラーを発生させる
			return errors.New("write failed on 2.in")
		}
		return nil
	}

	tc1 := NewTestCase(w)
	tc1.SetName("1")
	tc1.SetInput([]byte("in1"))
	tc1.SetWant([]byte("want1"))
	tc2 := NewTestCase(w)
	tc2.SetName("2")
	tc2.SetInput([]byte("in2"))
	tc2.SetWant([]byte("want2")) // この Write でエラー
	tc3 := NewTestCase(w)
	tc3.SetName("3")
	tc3.SetInput([]byte("in3"))
	tc3.SetWant([]byte("want3"))
	ts := TestCases{tc1, tc2, tc3}

	err := ts.Write()

	wantErrMsg := "write failed on 2.in"
	if err == nil || !strings.Contains(err.Error(), wantErrMsg) {
		t.Errorf("Write() error = %v, want error containing %q", err, wantErrMsg)
	}

	// tc1 のファイル書き込みが呼ばれたか
	if !writeCalls[tc1.InputFilePath()] {
		t.Errorf("tc1 input write was not called")
	}
	if !writeCalls[tc1.OutputFilePath()] {
		t.Errorf("tc1 output write was not called")
	}
	// tc2 のファイル書き込みが呼ばれたか (エラー発生箇所まで)
	if !writeCalls[tc2.InputFilePath()] {
		t.Errorf("tc2 input write was not called")
	}
	if writeCalls[tc2.OutputFilePath()] {
		t.Errorf("tc2 output write should not be called after input error")
	} // InputでエラーなのでOutputは呼ばれない
	// tc3 のファイル書き込みが呼ばれていないか
	if writeCalls[tc3.InputFilePath()] {
		t.Errorf("tc3 input write should not be called")
	}
	if writeCalls[tc3.OutputFilePath()] {
		t.Errorf("tc3 output write should not be called")
	}
}

func TestTestCases_Run(t *testing.T) {
	w := newTestWorkspace()
	runCount := 0
	w.runOsCmd = func(cmd []string, in io.Reader, out io.Writer, err io.Writer) error {
		runCount++
		if runCount == 2 { // tc2 の Run でエラー
			return errors.New("run failed on tc2")
		}
		// 正常な Run の動作（入力をコピー）
		_, e := io.Copy(out, in)
		return e
	}

	tc1 := NewTestCase(w)
	tc1.SetName("1")
	tc1.SetInput([]byte("in1"))
	tc2 := NewTestCase(w)
	tc2.SetName("2")
	tc2.SetInput([]byte("in2")) // この Run でエラー
	tc3 := NewTestCase(w)
	tc3.SetName("3")
	tc3.SetInput([]byte("in3"))
	ts := TestCases{tc1, tc2, tc3}

	lang := NewMockLang()
	err := ts.Run(lang)

	wantErrMsg := "run failed on tc2"
	if err == nil || !strings.Contains(err.Error(), wantErrMsg) {
		t.Errorf("Run() error = %v, want error containing %q", err, wantErrMsg)
	}

	// Run が呼ばれた回数を確認
	if runCount != 2 {
		t.Errorf("Language.Run called %d times, want 2", runCount)
	}
	// 各 TestCase の got が設定されているか確認
	if tc1.got == nil {
		t.Errorf("tc1.got is nil, Run should have set it")
	}
	if tc2.got != nil {
		t.Errorf("tc2.got should be nil because Run failed")
	}
	if tc3.got != nil {
		t.Errorf("tc3.got should be nil because Run stopped early")
	}
}

func TestTestCases_Report(t *testing.T) {
	w := newTestWorkspace()
	errBuf := &bytes.Buffer{}
	w.SetErr(errBuf)

	// tc1: 成功
	tc1 := NewTestCase(w)
	tc1.SetName("1")
	tc1.SetWant([]byte("pass"))
	tc1.got = []byte("pass")
	// tc2: 失敗
	tc2 := NewTestCase(w)
	tc2.SetName("2")
	tc2.SetInput([]byte("in"))
	tc2.SetWant([]byte("exp"))
	tc2.got = []byte("act")
	// tc3: 成功
	tc3 := NewTestCase(w)
	tc3.SetName("3")
	tc3.SetWant([]byte("pass"))
	tc3.got = []byte("pass")
	ts := TestCases{tc1, tc2, tc3}

	err := ts.Report()

	// Report はエラーを返さない
	if err != nil {
		t.Errorf("Report() error = %v, want nil", err)
	}

	// 標準エラー出力の内容を確認
	gotOutput := errBuf.String()
	if !strings.Contains(gotOutput, "✅ Test 1 passed") {
		t.Errorf("Report() output missing success for tc1: %q", gotOutput)
	}
	if !strings.Contains(gotOutput, "❌ Test 2 failed") {
		t.Errorf("Report() output missing failure for tc2: %q", gotOutput)
	}
	if !strings.Contains(gotOutput, "✅ Test 3 passed") {
		t.Errorf("Report() output missing success for tc3: %q", gotOutput)
	}
	// tc2 の詳細出力も確認
	if !strings.Contains(gotOutput, "Input:\nin") {
		t.Errorf("Report() output missing input detail for tc2: %q", gotOutput)
	}
	if !strings.Contains(gotOutput, "Expected:\nexp") {
		t.Errorf("Report() output missing want detail for tc2: %q", gotOutput)
	}
	if !strings.Contains(gotOutput, "Got:\nact") {
		t.Errorf("Report() output missing got detail for tc2: %q", gotOutput)
	}
}

package main

import (
	"bytes"
	"errors"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

// --- モック用のヘルパー ---

// testLogger は Logger インターフェースのテスト用実装です。
type testLogger struct {
	debugBuf bytes.Buffer
	infoBuf  bytes.Buffer
	warnBuf  bytes.Buffer
	errorBuf bytes.Buffer
}

func (l *testLogger) Debug(msg string, args ...any) {
	l.debugBuf.WriteString(msg)
}
func (l *testLogger) Info(msg string, args ...any) {
	l.infoBuf.WriteString(msg)
}
func (l *testLogger) Warn(msg string, args ...any) {
	l.warnBuf.WriteString(msg)
}
func (l *testLogger) Error(msg string, args ...any) {
	l.errorBuf.WriteString(msg)
}

// newTestWorkspace はテスト用の Workspace を作成します。
// 必要に応じてフィールドに関数を設定してモック化します。
func newTestWorkspace() *Workspace {
	return &Workspace{
		logger:      &testLogger{},
		testDir:     "test",
		cookiePath:  "/tmp/fake/home/.local/share/atcoder-cli/cookie.txt", // テスト用の固定パス
		readFile:    func(path string) ([]byte, error) { return nil, errors.New("readFile not implemented") },
		writeFile:   func(path string, data []byte, perm os.FileMode) error { return errors.New("writeFile not implemented") },
		getwd:       func() (string, error) { return "", errors.New("getwd not implemented") },
		getenv:      func(key string) string { return "" }, // デフォルトは空文字
		stat:        func(path string) (os.FileInfo, error) { return nil, errors.New("stat not implemented") },
		mkdir:       func(path string, perm os.FileMode) error { return errors.New("mkdir not implemented") },
		mkdirAll:    func(path string, perm os.FileMode) error { return errors.New("mkdirAll not implemented") },
		isExist:     os.IsExist,
		isNotExist:  os.IsNotExist,
		readDir:     func(path string) ([]os.DirEntry, error) { return nil, errors.New("readDir not implemented") },
		userHomeDir: func() (string, error) { return "/fake/home", nil }, // テスト用の固定ホームディレクトリ
		runOsCmd: func(cmd []string, stdin io.Reader, stdout, stderr io.Writer) error {
			return errors.New("runOsCmd not implemented")
		},
		inReader:  &bytes.Buffer{},
		outWriter: &bytes.Buffer{},
		errWriter: &bytes.Buffer{},
	}
}

// --- テストケース ---

func TestWorkspace_GetContestAndProblem(t *testing.T) {
	tests := []struct {
		name          string
		mockWd        string
		mockWdErr     error
		wantContest   string
		wantProblem   string
		wantErr       bool
		wantErrString string // エラーメッセージの内容も確認する場合
	}{
		{
			name:        "正常系: /path/to/contest/problem",
			mockWd:      "/path/to/abc100/a",
			wantContest: "abc100",
			wantProblem: "a",
			wantErr:     false,
		},
		{
			name:          "正常系: カレントディレクトリが /",
			mockWd:        "/",
			wantErr:       true,
			wantErrString: "no contest or problem found",
		},
		{
			name:          "正常系: カレントディレクトリが /contest",
			mockWd:        "/abc200",
			wantErr:       true,
			wantErrString: "no contest or problem found",
		},
		{
			name:          "異常系: getwd でエラー",
			mockWdErr:     errors.New("getwd failed"),
			wantErr:       true,
			wantErrString: "getwd failed",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			// getwd をモック化
			w.getwd = func() (string, error) {
				return tt.mockWd, tt.mockWdErr
			}

			contest, problem, err := w.GetContestAndProblem()

			if (err != nil) != tt.wantErr {
				t.Errorf("GetContestAndProblem() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr && tt.wantErrString != "" && !strings.Contains(err.Error(), tt.wantErrString) {
				t.Errorf("GetContestAndProblem() error = %v, wantErr containing %v", err, tt.wantErrString)
			}
			if contest != tt.wantContest {
				t.Errorf("GetContestAndProblem() contest = %v, want %v", contest, tt.wantContest)
			}
			if problem != tt.wantProblem {
				t.Errorf("GetContestAndProblem() problem = %v, want %v", problem, tt.wantProblem)
			}
		})
	}
}

func TestWorkspace_DetectLanguage(t *testing.T) {
	// モック用のファイル情報
	mockFileInfo := &mockFileInfo{name: "main.go", isDir: false}

	tests := []struct {
		name            string
		mockStatResults map[string]error // ファイル名 -> stat のエラー
		wantLangName    string
		wantErr         bool
		wantErrString   string
	}{
		{
			name: "Go ファイルが存在する",
			mockStatResults: map[string]error{
				"main.go":  nil, // 存在する場合のエラーは nil
				"main.cpp": os.ErrNotExist,
				"main.py":  os.ErrNotExist,
				"main.zig": os.ErrNotExist,
			},
			wantLangName: "go",
			wantErr:      false,
		},
		{
			name: "Cpp ファイルが存在する",
			mockStatResults: map[string]error{
				"main.go":  os.ErrNotExist,
				"main.cpp": nil,
				"main.py":  os.ErrNotExist,
				"main.zig": os.ErrNotExist,
			},
			wantLangName: "cpp",
			wantErr:      false,
		},
		{
			name: "Python ファイルが存在する",
			mockStatResults: map[string]error{
				"main.go":  os.ErrNotExist,
				"main.cpp": os.ErrNotExist,
				"main.py":  nil,
				"main.zig": os.ErrNotExist,
			},
			wantLangName: "python",
			wantErr:      false,
		},
		{
			name: "Zig ファイルが存在する",
			mockStatResults: map[string]error{
				"main.go":  os.ErrNotExist,
				"main.cpp": os.ErrNotExist,
				"main.py":  os.ErrNotExist,
				"main.zig": nil,
			},
			wantLangName: "zig",
			wantErr:      false,
		},
		{
			name: "どのファイルも存在しない",
			mockStatResults: map[string]error{
				"main.go":  os.ErrNotExist,
				"main.cpp": os.ErrNotExist,
				"main.py":  os.ErrNotExist,
				"main.zig": os.ErrNotExist,
			},
			wantErr:       true,
			wantErrString: "mainファイルが見つかりません",
		},
		{
			name: "stat で予期せぬエラーが発生する",
			mockStatResults: map[string]error{
				"main.go":  errors.New("some stat error"), // 予期せぬエラー
				"main.cpp": os.ErrNotExist,
				"main.py":  os.ErrNotExist,
				"main.zig": os.ErrNotExist,
			},
			wantErr:       true,               // エラーが返されることを期待
			wantErrString: "mainファイルが見つかりません", // 最終的に見つからないエラーになる
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			// stat をモック化
			w.stat = func(path string) (os.FileInfo, error) {
				err, ok := tt.mockStatResults[path]
				if !ok {
					// マップにないファイルは存在しない扱い
					return nil, os.ErrNotExist
				}
				if err == nil {
					// エラーがない場合はモックファイル情報を返す
					return mockFileInfo, nil
				}
				return nil, err // 設定されたエラーを返す
			}

			lang, err := w.DetectLanguage()

			if (err != nil) != tt.wantErr {
				t.Errorf("DetectLanguage() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr {
				if tt.wantErrString != "" && !strings.Contains(err.Error(), tt.wantErrString) {
					t.Errorf("DetectLanguage() error = %q, wantErr containing %q", err.Error(), tt.wantErrString)
				}
			} else {
				// エラーがない場合は言語名を比較
				if lang == nil {
					t.Errorf("DetectLanguage() lang = nil, want %v", tt.wantLangName)
				} else if lang.Name != tt.wantLangName {
					t.Errorf("DetectLanguage() lang.Name = %v, want %v", lang.Name, tt.wantLangName)
				}
			}
		})
	}
}

// --- モック os.FileInfo ---
type mockFileInfo struct {
	name  string
	size  int64
	mode  os.FileMode
	isDir bool
}

func (m *mockFileInfo) Name() string       { return m.name }
func (m *mockFileInfo) Size() int64        { return m.size }
func (m *mockFileInfo) Mode() os.FileMode  { return m.mode }
func (m *mockFileInfo) ModTime() time.Time { return time.Time{} } // ダミー
func (m *mockFileInfo) IsDir() bool        { return m.isDir }
func (m *mockFileInfo) Sys() interface{}   { return nil }

// --- モック os.DirEntry ---
type mockDirEntry struct {
	name  string
	isDir bool
	typ   os.FileMode
	info  os.FileInfo
	err   error
}

func (m *mockDirEntry) Name() string               { return m.name }
func (m *mockDirEntry) IsDir() bool                { return m.isDir }
func (m *mockDirEntry) Type() os.FileMode          { return m.typ }
func (m *mockDirEntry) Info() (os.FileInfo, error) { return m.info, m.err }

func TestWorkspace_FetchTestCases(t *testing.T) {
	tests := []struct {
		name           string
		mockReadDir    []os.DirEntry
		mockReadDirErr error
		wantTestCases  TestCases
		wantErr        bool
	}{
		{
			name: "正常系: .in ファイルが複数存在する",
			mockReadDir: []os.DirEntry{
				&mockDirEntry{name: "1.in"},
				&mockDirEntry{name: "1.out"}, // .out は無視される
				&mockDirEntry{name: "2.in"},
				&mockDirEntry{name: "sample-3.in"},
				&mockDirEntry{name: "other.txt"}, // .txt は無視される
			},
			wantTestCases: TestCases{
				{name: "1"},
				{name: "2"},
				{name: "sample-3"},
			},
			wantErr: false,
		},
		{
			name: "正常系: .in ファイルが存在しない",
			mockReadDir: []os.DirEntry{
				&mockDirEntry{name: "1.out"},
				&mockDirEntry{name: "other.txt"},
			},
			wantTestCases: TestCases{}, // 空のスライス
			wantErr:       false,
		},
		{
			name:          "正常系: ディレクトリが空",
			mockReadDir:   []os.DirEntry{},
			wantTestCases: TestCases{},
			wantErr:       false,
		},
		{
			name:           "異常系: readDir でエラー",
			mockReadDirErr: errors.New("readDir failed"),
			wantErr:        true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			// readDir をモック化
			w.readDir = func(path string) ([]os.DirEntry, error) {
				if path != w.testDir { // testDir 以外へのアクセスはエラーにする（念のため）
					return nil, errors.New("unexpected directory access")
				}
				return tt.mockReadDir, tt.mockReadDirErr
			}

			// TestCase の比較のために Workspace を設定
			// (NewTestCase 内で w が使われるため)
			for _, tc := range tt.wantTestCases {
				tc.w = w
			}

			ts, err := w.FetchTestCases()

			if (err != nil) != tt.wantErr {
				t.Errorf("FetchTestCases() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr {
				// エラーの場合はこれ以上比較しない
				return
			}

			// TestCase スライスの比較 (順序も考慮)
			// TestCase 構造体は比較できないフィールド(関数ポインタなど)を含む可能性があるため、
			// 必要なフィールド（ここでは Name）だけを比較する
			if len(ts) != len(tt.wantTestCases) {
				t.Errorf("FetchTestCases() len = %d, want %d", len(ts), len(tt.wantTestCases))
				t.Logf("Got: %v", ts)
				t.Logf("Want: %v", tt.wantTestCases)
				return
			}
			for i := range ts {
				if ts[i].Name() != tt.wantTestCases[i].Name() {
					t.Errorf("FetchTestCases()[%d].Name() = %q, want %q", i, ts[i].Name(), tt.wantTestCases[i].Name())
				}
				// 必要であれば他のフィールドも比較する
				// 例: if !reflect.DeepEqual(ts[i].input, tt.wantTestCases[i].input) { ... }
			}
		})
	}
}

// 他のメソッド (LoadCookies, SaveCookie, PromptCookie など) のテストも同様に追加できます。
// ファイル I/O や標準入出力、環境変数などを適切にモック化してください。

func TestWorkspace_LoadCookies(t *testing.T) {
	tests := []struct {
		name            string
		cookieFilePath  string
		mockStatErr     error
		mockReadFile    []byte
		mockReadFileErr error
		wantCookies     []*http.Cookie
		wantErr         bool
	}{
		{
			name:           "正常系: クッキーファイルが存在し、有効な内容",
			cookieFilePath: "/path/to/cookie.txt",
			mockStatErr:    nil, // ファイルが存在する
			mockReadFile:   []byte("REVEL_SESSION=session_value\nANOTHER_COOKIE=another_value\n"),
			wantCookies: []*http.Cookie{
				{Name: "REVEL_SESSION", Value: "session_value"},
				{Name: "ANOTHER_COOKIE", Value: "another_value"},
			},
			wantErr: false,
		},
		{
			name:           "正常系: クッキーファイルが存在し、空",
			cookieFilePath: "/path/to/empty.txt",
			mockStatErr:    nil,
			mockReadFile:   []byte(""),
			wantCookies:    nil, // 空のスライスではなく nil を期待
			wantErr:        false,
		},
		{
			name:           "正常系: クッキーファイルが存在し、最後の行が空行",
			cookieFilePath: "/path/to/cookie_emptyline.txt",
			mockStatErr:    nil,
			mockReadFile:   []byte("KEY=VALUE\n"),
			wantCookies: []*http.Cookie{
				{Name: "KEY", Value: "VALUE"},
			},
			wantErr: false,
		},
		{
			name:           "正常系: クッキーファイルが存在しない",
			cookieFilePath: "/path/to/not_exist.txt",
			mockStatErr:    os.ErrNotExist, // ファイルが存在しない
			wantCookies:    nil,
			wantErr:        false, // ファイルが存在しないのはエラーではない
		},
		{
			name:           "異常系: stat で予期せぬエラー",
			cookieFilePath: "/path/to/stat_error.txt",
			mockStatErr:    errors.New("stat failed"),
			wantCookies:    nil,
			wantErr:        false, // stat のエラーは無視され、ファイルが存在しない場合と同じ扱いになる
		},
		{
			name:            "異常系: ReadFile でエラー",
			cookieFilePath:  "/path/to/read_error.txt",
			mockStatErr:     nil,
			mockReadFileErr: errors.New("read failed"),
			wantCookies:     nil,
			wantErr:         true, // ReadFile のエラーは返される
		},
		{
			name:           "正常系: 不正な形式の行は無視される",
			cookieFilePath: "/path/to/invalid_lines.txt",
			mockStatErr:    nil,
			mockReadFile:   []byte("VALID=1\nINVALID_LINE\n=ONLY_VALUE\nKEY_ONLY=\nVALID2=2\n"),
			wantCookies: []*http.Cookie{
				{Name: "VALID", Value: "1"},
				{Name: "KEY_ONLY", Value: ""}, // 値が空のクッキー
				{Name: "VALID2", Value: "2"},
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			w.cookiePath = tt.cookieFilePath // テストケースごとに cookiePath を設定

			// stat と readFile をモック化
			w.stat = func(path string) (os.FileInfo, error) {
				if path == tt.cookieFilePath {
					// stat は isNotExist を正しく判定するために os.ErrNotExist を返す必要がある
					if tt.mockStatErr == os.ErrNotExist {
						return nil, os.ErrNotExist
					}
					// それ以外のエラーや成功時は適当な FileInfo とエラーを返す
					if tt.mockStatErr != nil {
						return nil, tt.mockStatErr
					}
					return &mockFileInfo{}, nil // 成功時は nil エラー
				}
				return nil, errors.New("unexpected stat call")
			}
			w.readFile = func(path string) ([]byte, error) {
				if path == tt.cookieFilePath {
					return tt.mockReadFile, tt.mockReadFileErr
				}
				return nil, errors.New("unexpected readFile call")
			}

			cookies, err := w.LoadCookies()

			if (err != nil) != tt.wantErr {
				t.Errorf("LoadCookies() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr {
				return // エラーの場合はクッキーの内容比較はしない
			}

			// クッキースライスの比較 (順序も考慮)
			if !reflect.DeepEqual(cookies, tt.wantCookies) {
				t.Errorf("LoadCookies() cookies = %v, want %v", cookies, tt.wantCookies)
			}
		})
	}
}

func TestWorkspace_SaveCookie(t *testing.T) {
	tests := []struct {
		name             string
		cookieToSave     string
		mockMkdirAllErr  error
		mockWriteFileErr error
		wantWriteData    []byte
		wantWritePerm    os.FileMode
		wantErr          bool
	}{
		{
			name:          "正常系",
			cookieToSave:  "REVEL_SESSION=test_value",
			wantWriteData: []byte("REVEL_SESSION=test_value\n"), // 末尾に改行が追加される
			wantWritePerm: 0600,
			wantErr:       false,
		},
		{
			name:            "異常系: MkdirAll でエラー",
			cookieToSave:    "REVEL_SESSION=test_value",
			mockMkdirAllErr: errors.New("mkdir failed"),
			wantErr:         true,
		},
		{
			name:             "異常系: WriteFile でエラー",
			cookieToSave:     "REVEL_SESSION=test_value",
			mockWriteFileErr: errors.New("write failed"),
			wantWriteData:    []byte("REVEL_SESSION=test_value\n"),
			wantWritePerm:    0600,
			wantErr:          true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			// cookiePath のディレクトリ部分を抽出
			cookieDir := filepath.Dir(w.cookiePath)

			var mkdirCalled bool
			var writeFileCalled bool
			var gotWritePath string
			var gotWriteData []byte
			var gotWritePerm os.FileMode

			// mkdirAll と writeFile をモック化
			w.mkdirAll = func(path string, perm os.FileMode) error {
				if path == cookieDir && perm == 0755 { // MkPubDir は 0755 で呼ばれる
					mkdirCalled = true
					return tt.mockMkdirAllErr
				}
				return errors.New("unexpected mkdirAll call")
			}
			w.writeFile = func(path string, data []byte, perm os.FileMode) error {
				if path == w.cookiePath {
					writeFileCalled = true
					gotWritePath = path
					gotWriteData = append([]byte{}, data...) // スライスをコピーして保存
					gotWritePerm = perm
					return tt.mockWriteFileErr
				}
				return errors.New("unexpected writeFile call")
			}

			err := w.SaveCookie(tt.cookieToSave)

			if (err != nil) != tt.wantErr {
				t.Errorf("SaveCookie() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr {
				// エラーの場合でも、エラー発生箇所によっては関数が呼ばれているか確認
				if tt.mockMkdirAllErr == nil && !mkdirCalled {
					t.Error("SaveCookie() did not call mkdirAll when expected")
				}
				if tt.mockMkdirAllErr == nil && tt.mockWriteFileErr != nil && !writeFileCalled {
					t.Error("SaveCookie() did not call writeFile when expected")
				}
				return
			}

			// 正常系の場合、関数呼び出しと引数を確認
			if !mkdirCalled {
				t.Error("SaveCookie() did not call mkdirAll")
			}
			if !writeFileCalled {
				t.Error("SaveCookie() did not call writeFile")
			}
			if gotWritePath != w.cookiePath {
				t.Errorf("SaveCookie() wrote to path %q, want %q", gotWritePath, w.cookiePath)
			}
			if !bytes.Equal(gotWriteData, tt.wantWriteData) {
				t.Errorf("SaveCookie() wrote data %q, want %q", string(gotWriteData), string(tt.wantWriteData))
			}
			if gotWritePerm != tt.wantWritePerm {
				t.Errorf("SaveCookie() wrote with perm %v, want %v", gotWritePerm, tt.wantWritePerm)
			}
		})
	}
}

func TestWorkspace_PromptCookie(t *testing.T) {
	tests := []struct {
		name        string
		mockInput   string
		wantOutput  string // 標準エラーへの出力
		wantCookie  string
		wantErr     bool
		mockReadErr error
	}{
		{
			name:       "正常系: 通常の入力",
			mockInput:  "my_revel_session_value\n",
			wantOutput: "REVEL_SESSION cookie を入力してください: \n",
			wantCookie: "REVEL_SESSION=my_revel_session_value",
			wantErr:    false,
		},
		{
			name:       "正常系: 入力の前後に空白",
			mockInput:  "  spaced_value  \n",
			wantOutput: "REVEL_SESSION cookie を入力してください: \n",
			wantCookie: "REVEL_SESSION=spaced_value", // TrimSpace される
			wantErr:    false,
		},
		{
			name:       "正常系: 空の入力",
			mockInput:  "\n",
			wantOutput: "REVEL_SESSION cookie を入力してください: \n",
			wantCookie: "REVEL_SESSION=", // 空文字になる
			wantErr:    false,
		},
		{
			name:        "異常系: ReadString でエラー",
			mockInput:   "some input", // エラーなので改行は関係ない
			wantOutput:  "REVEL_SESSION cookie を入力してください: \n",
			wantErr:     true,
			mockReadErr: errors.New("read error"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			inBuf := bytes.NewBufferString(tt.mockInput)
			errBuf := &bytes.Buffer{}

			// 標準入力と標準エラーをモック化
			// ただし、bufio.Reader が内部で使われるため、ReadString を直接モック化するのではなく、
			// w.GetIn() が返す io.Reader を差し替える。
			// ReadString のエラーをシミュレートするために、エラーを返す Reader を使う。
			if tt.mockReadErr != nil {
				w.SetIn(&errorReader{err: tt.mockReadErr})
			} else {
				w.SetIn(inBuf)
			}
			w.SetErr(errBuf)

			cookie, err := w.PromptCookie()

			// 標準エラーへの出力を検証
			if gotOutput := errBuf.String(); gotOutput != tt.wantOutput {
				t.Errorf("PromptCookie() output = %q, want %q", gotOutput, tt.wantOutput)
			}

			if (err != nil) != tt.wantErr {
				t.Errorf("PromptCookie() error = %v, wantErr %v", err, tt.wantErr)
				// ReadString のエラーの場合、err の内容は io.Reader の実装依存になるため、
				// エラーメッセージの完全一致比較は難しい場合がある。
				// ここではエラーの有無のみを確認する。
				return
			}
			if tt.wantErr {
				return
			}

			if cookie != tt.wantCookie {
				t.Errorf("PromptCookie() cookie = %q, want %q", cookie, tt.wantCookie)
			}
		})
	}
}

// errorReader は Read で指定されたエラーを返す io.Reader
type errorReader struct {
	err error
}

func (r *errorReader) Read(p []byte) (n int, err error) {
	return 0, r.err
}

func Test_prodCookiePath(t *testing.T) {
	tests := []struct {
		name        string
		mockXdgHome string
		mockHome    string
		mockHomeErr error
		wantPath    string
	}{
		{
			name:        "XDG_DATA_HOME が設定されている",
			mockXdgHome: "/xdg/data",
			mockHome:    "/user/home", // 使われないはず
			wantPath:    "/xdg/data/atcoder-cli/cookie.txt",
		},
		{
			name:        "XDG_DATA_HOME が空文字、HOME が設定されている",
			mockXdgHome: "",
			mockHome:    "/user/home",
			wantPath:    "/user/home/.local/share/atcoder-cli/cookie.txt",
		},
		{
			name:        "XDG_DATA_HOME が空文字、HOME も空文字 (userHomeDir がエラーを返す場合)",
			mockXdgHome: "",
			mockHomeErr: errors.New("home dir error"),
			// エラーの場合、home は空文字列になるので、/.local/share/... となる
			wantPath: filepath.Join(".local", "share", "atcoder-cli", "cookie.txt"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Workspace を一時的に作成してモック関数を設定
			w := newTestWorkspace()
			w.getenv = func(key string) string {
				if key == "XDG_DATA_HOME" {
					return tt.mockXdgHome
				}
				return "" // 他の環境変数は空とする
			}
			w.userHomeDir = func() (string, error) {
				return tt.mockHome, tt.mockHomeErr
			}

			gotPath := prodCookiePath(w)

			// filepath.Join は OS によってパス区切り文字が変わるため、
			// 期待値も filepath.Join を使って生成する方が安全
			wantPath := tt.wantPath
			// Windows 以外では / 区切りで比較しても良いが、Join を使うのが確実
			if !filepath.IsAbs(wantPath) && strings.HasPrefix(wantPath, ".local") {
				// 相対パスの場合の期待値を Join で生成
				wantPath = filepath.Join(".local", "share", "atcoder-cli", "cookie.txt")
			} else if filepath.IsAbs(wantPath) && strings.Contains(wantPath, "/.local/share") {
				// 絶対パスの場合の期待値を Join で生成
				wantPath = filepath.Join(tt.mockHome, ".local", "share", "atcoder-cli", "cookie.txt")
			} else if filepath.IsAbs(wantPath) && strings.HasPrefix(wantPath, tt.mockXdgHome) {
				wantPath = filepath.Join(tt.mockXdgHome, "atcoder-cli", "cookie.txt")
			}

			if gotPath != wantPath {
				t.Errorf("prodCookiePath() = %q, want %q", gotPath, wantPath)
			}
		})
	}
}

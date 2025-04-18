package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

// --- Test parseInitCmdOpt ---

func Test_parseInitCmdOpt(t *testing.T) {
	tests := []struct {
		name          string
		args          []string
		wantOpt       InitCmdOpt
		wantErr       bool
		wantErrString string
	}{
		{
			name:    "正常系",
			args:    []string{"abc100"},
			wantOpt: InitCmdOpt{contest: "abc100"},
			wantErr: false,
		},
		{
			name:          "引数なし",
			args:          []string{},
			wantErr:       true,
			wantErrString: "コンテスト名を指定してください",
		},
		{
			name:    "引数が複数 (最初の引数のみ使用)",
			args:    []string{"abc200", "extra"},
			wantOpt: InitCmdOpt{contest: "abc200"},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotOpt, err := parseInitCmdOpt(tt.args)

			if (err != nil) != tt.wantErr {
				t.Errorf("parseInitCmdOpt() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr && tt.wantErrString != "" && !strings.Contains(err.Error(), tt.wantErrString) {
				t.Errorf("parseInitCmdOpt() error = %q, wantErr containing %q", err.Error(), tt.wantErrString)
			}
			if !tt.wantErr && !reflect.DeepEqual(gotOpt, tt.wantOpt) {
				t.Errorf("parseInitCmdOpt() = %v, want %v", gotOpt, tt.wantOpt)
			}
		})
	}
}

// newTestCommand はテスト用の Command と BaseCommand を作成します。
func newTestCommand[T any](opt *NewCommandOpt[T]) *Command[T] {
	// NewCommand を呼び出して基本的な設定を行う
	cmd := NewCommand(opt)
	// テスト用に Workspace を設定
	cmd.SetWorkspace(newTestWorkspace())
	return cmd
}

// --- Test InitCmdRunner ---

func TestInitCmdRunner_Run(t *testing.T) {
	tests := []struct {
		name             string
		opt              InitCmdOpt
		mockProblemIDs   []string
		mockGetIDsErr    error
		mockMkdirErr     error
		mockMkdirErrPath string // Mkdir でエラーを発生させるパス
		wantMkdirPaths   []string
		wantExitCode     ExitCode
		wantErrMsg       string // Workspace.Error に出力されるメッセージの一部
	}{
		{
			name:           "正常系",
			opt:            InitCmdOpt{contest: "abc100"},
			mockProblemIDs: []string{"a", "b"},
			wantMkdirPaths: []string{
				"abc100",
				filepath.Join("abc100", "a"),
				filepath.Join("abc100", "b"),
			},
			wantExitCode: ExitOK,
		},
		{
			name:           "正常系 (問題IDなし)",
			opt:            InitCmdOpt{contest: "arc100"},
			mockProblemIDs: []string{},
			wantMkdirPaths: []string{
				"arc100", // コンテストディレクトリは作成される
			},
			wantExitCode: ExitOK,
		},
		{
			name:           "GetProblemIDs でエラー",
			opt:            InitCmdOpt{contest: "abc200"},
			mockGetIDsErr:  errors.New("failed to fetch IDs"),
			wantMkdirPaths: []string{},
			wantExitCode:   ExitError,
			wantErrMsg:     "failed to fetch IDs",
		},
		{
			name:             "Mkdir でエラー (コンテストディレクトリ)",
			opt:              InitCmdOpt{contest: "abc300"},
			mockProblemIDs:   []string{"a"},
			mockMkdirErr:     errors.New("mkdir contest failed"),
			mockMkdirErrPath: "abc300",
			wantMkdirPaths: []string{
				"abc300", // 呼び出しはされる
			},
			wantExitCode: ExitError,
			wantErrMsg:   "mkdir contest failed",
		},
		{
			name:             "Mkdir でエラー (問題ディレクトリ)",
			opt:              InitCmdOpt{contest: "abc400"},
			mockProblemIDs:   []string{"a", "b"},
			mockMkdirErr:     errors.New("mkdir problem b failed"),
			mockMkdirErrPath: filepath.Join("abc400", "b"), // b のディレクトリ作成で失敗
			wantMkdirPaths: []string{
				"abc400",
				filepath.Join("abc400", "a"),
				filepath.Join("abc400", "b"), // 呼び出しはされる
			},
			wantExitCode: ExitError,
			wantErrMsg:   "mkdir problem b failed",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			a, mockTransport := newMockAtcoder(w) // Atcoder のモックも取得
			runner := &InitCmdRunner{w: w, atcoder: a}
			errBuf := &bytes.Buffer{}
			w.SetErr(errBuf) // エラー出力をキャプチャ

			// Atcoder.GetProblemIDs のモック設定
			targetPath := fmt.Sprintf("/contests/%s/tasks", tt.opt.contest)
			mockTransport.RoundTripFunc = func(req *http.Request) (*http.Response, error) {
				if tt.mockGetIDsErr != nil {
					return nil, tt.mockGetIDsErr
				}
				if req.URL.Path != targetPath {
					return nil, fmt.Errorf("unexpected request path: %s, want %s", req.URL.Path, targetPath)
				}
				// GetProblemIDs は parseProblemIDs を呼ぶので、それを模倣する HTML を返す
				var body string
				for _, id := range tt.mockProblemIDs {
					body += fmt.Sprintf(`<a href="/contests/%s/tasks/%s_%s">Problem %s</a>`, tt.opt.contest, tt.opt.contest, id, strings.ToUpper(id))
				}
				resp := &http.Response{
					StatusCode: http.StatusOK,
					Body:       io.NopCloser(strings.NewReader(body)),
					Request:    req,
					Header:     make(http.Header),
				}
				return resp, nil
			}

			// Workspace.MkPubDir のモック設定
			mkdirCalledPaths := []string{}
			w.mkdir = func(path string, perm os.FileMode) error {
				// MkPubDir は内部で Mkdir(path, 0755) を呼ぶ
				if perm != 0755 {
					t.Errorf("MkPubDir called mkdir with perm %v, want 0755", perm)
				}
				mkdirCalledPaths = append(mkdirCalledPaths, path)
				if tt.mockMkdirErr != nil && path == tt.mockMkdirErrPath {
					return tt.mockMkdirErr
				}
				return nil // 成功
			}
			// isExist は常に false を返すようにして、ディレクトリ存在済みエラーを無視させない
			w.isExist = func(err error) bool { return false }

			exitCode := runner.Run(tt.opt)

			if exitCode != tt.wantExitCode {
				t.Errorf("Run() exitCode = %d, want %d", exitCode, tt.wantExitCode)
			}

			// MkPubDir (内部の mkdir) が期待通り呼ばれたか確認
			if !reflect.DeepEqual(mkdirCalledPaths, tt.wantMkdirPaths) {
				t.Errorf("MkPubDir called with paths = %v, want %v", mkdirCalledPaths, tt.wantMkdirPaths)
			}

			// エラーメッセージの確認 (標準エラーではなく、ロガーのエラーバッファを確認)
			testLogger, ok := w.GetLogger().(*testLogger)
			if !ok {
				t.Fatal("Workspace logger is not *testLogger")
			}
			gotErrMsg := testLogger.errorBuf.String() // ロガーのエラーバッファを取得

			if tt.wantErrMsg != "" {
				// if gotErrMsg := errBuf.String(); !strings.Contains(gotErrMsg, tt.wantErrMsg) { // 元のコード (errBuf を見ていた)
				if !strings.Contains(gotErrMsg, tt.wantErrMsg) { // 修正後 (gotErrMsg を使用)
					t.Errorf("Run() logger error = %q, want containing %q", gotErrMsg, tt.wantErrMsg)
				}
			} else if tt.wantExitCode == ExitOK {
				// 正常終了時はエラー出力がないはず (ロガーにも標準エラーにも)
				if gotErrMsg != "" { // ロガーのエラーバッファを確認
					t.Errorf("Run() expected no logger error output on success, but got %q", gotErrMsg)
				}
				if stdErrOutput := errBuf.String(); stdErrOutput != "" { // 標準エラーも念のため確認
					t.Errorf("Run() expected no stderr output on success, but got %q", stdErrOutput)
				}
			}
		})
	}
}

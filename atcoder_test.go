package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"os"
	"reflect"
	"strings"
	"testing"
)

// --- HTTP Client モック ---

// mockRoundTripper は http.RoundTripper インターフェースのモック実装です。
type mockRoundTripper struct {
	// RoundTripFunc は RoundTrip が呼ばれたときに実行される関数です。
	// これを設定することで、リクエストに応じたレスポンスやエラーを返すことができます。
	RoundTripFunc func(req *http.Request) (*http.Response, error)
	// Requests は RoundTrip に渡されたリクエストを記録します。
	Requests []*http.Request
}

func (m *mockRoundTripper) RoundTrip(req *http.Request) (*http.Response, error) {
	m.Requests = append(m.Requests, req) // リクエストを記録
	if m.RoundTripFunc != nil {
		return m.RoundTripFunc(req)
	}
	// デフォルトではエラーを返すか、空のレスポンスを返すなど
	return nil, errors.New("mockRoundTripper.RoundTripFunc not set")
}

// newMockAtcoder はテスト用の Atcoder インスタンスとモック RoundTripper を作成します。
func newMockAtcoder(w *Workspace) (*Atcoder, *mockRoundTripper) {
	if w == nil {
		w = newTestWorkspace() // Workspace が nil ならテスト用を作成
	}
	mockTransport := &mockRoundTripper{}
	jar, _ := cookiejar.New(nil) // テスト用の空の Cookie Jar

	// LoadCookies が呼ばれたときに Jar に設定されるようにモックを設定
	w.readFile = func(path string) ([]byte, error) {
		if path == w.cookiePath {
			// テストケースに応じてクッキーファイルの内容を返す
			return nil, os.ErrNotExist // デフォルトは存在しない
		}
		return nil, fmt.Errorf("unexpected readFile call: %s", path)
	}
	w.stat = func(path string) (os.FileInfo, error) {
		if path == w.cookiePath {
			return nil, os.ErrNotExist // デフォルトは存在しない
		}
		return nil, fmt.Errorf("unexpected stat call: %s", path)
	}

	client := &http.Client{
		Transport: mockTransport,
		Jar:       jar,
	}
	atcoderURL, _ := url.Parse("https://atcoder.jp")

	atcoder := &Atcoder{
		url:    atcoderURL,
		client: client,
		w:      w,
	}
	// 初期状態で ReloadCookies を呼んでおく (NewAtcoder の動作を模倣)
	_ = atcoder.ReloadCookies()

	return atcoder, mockTransport
}

// --- テストケース ---

func TestAtcoder_ReloadCookies(t *testing.T) {
	tests := []struct {
		name           string
		mockCookieData []byte
		mockCookieErr  error
		wantCookieName string // Jar に設定されることを期待するクッキー名
		wantErr        bool
	}{
		{
			name:           "クッキーファイルが存在し、読み込み成功",
			mockCookieData: []byte("REVEL_SESSION=testsession\n"),
			wantCookieName: "REVEL_SESSION",
			wantErr:        false,
		},
		{
			name:           "クッキーファイルが存在しない",
			mockCookieErr:  os.ErrNotExist, // readFile が返すエラー
			wantCookieName: "",             // 何も設定されない
			wantErr:        false,          // ファイルなしはエラーではない
		},
		{
			name:          "クッキーファイルの読み込みエラー",
			mockCookieErr: errors.New("read failed"),
			wantErr:       true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			w := newTestWorkspace()
			a, _ := newMockAtcoder(w) // モック Transport は使わない

			// LoadCookies のためのモック設定
			w.readFile = func(path string) ([]byte, error) {
				if path == w.cookiePath {
					return tt.mockCookieData, tt.mockCookieErr
				}
				return nil, fmt.Errorf("unexpected readFile call: %s", path)
			}
			w.stat = func(path string) (os.FileInfo, error) {
				if path == w.cookiePath {
					if tt.mockCookieErr == os.ErrNotExist {
						return nil, os.ErrNotExist
					}
					if tt.mockCookieErr != nil {
						return nil, errors.New("stat error for read error case") // ReadFileエラー時のstatは成功させる
					}
					return &mockFileInfo{}, nil // 存在する
				}
				return nil, fmt.Errorf("unexpected stat call: %s", path)
			}

			err := a.ReloadCookies()

			if (err != nil) != tt.wantErr {
				t.Errorf("ReloadCookies() error = %v, wantErr %v", err, tt.wantErr)
			}

			if !tt.wantErr {
				cookies := a.client.Jar.Cookies(a.url)
				found := false
				for _, c := range cookies {
					if c.Name == tt.wantCookieName {
						found = true
						break
					}
				}
				if tt.wantCookieName != "" && !found {
					t.Errorf("Cookie %q not found in Jar after ReloadCookies()", tt.wantCookieName)
				}
				if tt.wantCookieName == "" && len(cookies) > 0 {
					t.Errorf("Expected no cookies in Jar, but found %d", len(cookies))
				}
			}
		})
	}
}

func TestAtcoder_LoginCheck(t *testing.T) {
	tests := []struct {
		name           string
		mockStatusCode int
		mockRedirectTo string // リダイレクト先のパス (リダイレクトしない場合は空)
		mockGetErr     error
		wantLoggedIn   bool
		wantErr        bool
	}{
		{
			name:           "ログイン成功 (ステータスコード 200, リダイレクトなし)",
			mockStatusCode: http.StatusOK,
			wantLoggedIn:   true,
			wantErr:        false,
		},
		{
			name:           "未ログイン (リダイレクト発生)",
			mockStatusCode: http.StatusOK, // リダイレクト前のステータスは問わないことが多い
			mockRedirectTo: "/login",      // ログインページにリダイレクトされる想定
			wantLoggedIn:   false,
			wantErr:        false,
		},
		{
			name:           "HTTPエラー (500など)",
			mockStatusCode: http.StatusInternalServerError,
			wantLoggedIn:   false,
			wantErr:        true, // HTTPエラーはエラーとして扱う
		},
		{
			name:         "ネットワークエラー",
			mockGetErr:   errors.New("network error"),
			wantLoggedIn: false,
			wantErr:      true,
		},
	}

	targetPath := "/contests/abc001/submit"

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a, mockTransport := newMockAtcoder(nil)

			mockTransport.RoundTripFunc = func(req *http.Request) (*http.Response, error) {
				if tt.mockGetErr != nil {
					return nil, tt.mockGetErr
				}
				// リクエストパスの検証
				if req.URL.Path != targetPath {
					return nil, fmt.Errorf("unexpected request path: %s, want %s", req.URL.Path, targetPath)
				}

				resp := &http.Response{
					StatusCode: tt.mockStatusCode,
					Body:       io.NopCloser(bytes.NewBufferString("")),
					Request:    req, // レスポンスにリクエスト情報を設定
					Header:     make(http.Header),
				}
				// リダイレクト先の URL を設定
				if tt.mockRedirectTo != "" {
					// 実際の HTTP クライアントはリダイレクトを自動で追うが、
					// モックではリダイレクトが発生したことを示すために、
					// レスポンスの Request.URL をリダイレクト後のものに書き換える
					redirectURL, _ := url.Parse(a.url.String() + tt.mockRedirectTo)
					resp.Request.URL = redirectURL
				}
				return resp, nil
			}

			loggedIn, err := a.LoginCheck()

			if (err != nil) != tt.wantErr {
				t.Errorf("LoginCheck() error = %v, wantErr %v", err, tt.wantErr)
			}
			if loggedIn != tt.wantLoggedIn {
				t.Errorf("LoginCheck() loggedIn = %v, want %v", loggedIn, tt.wantLoggedIn)
			}
		})
	}
}

func TestAtcoder_parseTestCases(t *testing.T) {
	tests := []struct {
		name      string
		html      string
		wantCases TestCases
		wantErr   bool
	}{
		{
			name: "正常系: 1ケース",
			html: `
				<h3>入力例 1</h3>
				<pre>1 2</pre>
				<h3>出力例 1</h3>
				<pre>3</pre>
			`,
			wantCases: TestCases{
				{name: "1", input: []byte("1 2"), want: []byte("3")},
			},
			wantErr: false,
		},
		{
			name: "正常系: 複数ケース、空白含む",
			html: `
				<h3>入力例 1</h3> <pre> 10 20 </pre>
				<h3>出力例 1</h3>   <pre>30
</pre>
				<h3>入力例 2</h3><pre> -5 5 </pre>
				<h3>出力例 2</h3><pre>0</pre>
			`,
			wantCases: TestCases{
				{name: "1", input: []byte("10 20"), want: []byte("30")},
				{name: "2", input: []byte("-5 5"), want: []byte("0")},
			},
			wantErr: false,
		},
		{
			name:    "異常系: 入力のみ",
			html:    `<h3>入力例 1</h3><pre>data</pre>`,
			wantErr: true,
		},
		{
			name:    "異常系: 出力のみ",
			html:    `<h3>出力例 1</h3><pre>data</pre>`,
			wantErr: true,
		},
		{
			name: "異常系: 入力と出力の順番が逆",
			html: `
				<h3>出力例 1</h3><pre>out</pre>
				<h3>入力例 1</h3><pre>in</pre>
			`,
			wantErr: true,
		},
		{
			name:    "異常系: HTML が空",
			html:    "",
			wantErr: false, // 空の TestCases が返る
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a, _ := newMockAtcoder(nil) // Workspace は NewTestCase で使われる
			reader := strings.NewReader(tt.html)

			gotCases, err := a.parseTestCases(reader)

			if (err != nil) != tt.wantErr {
				t.Errorf("parseTestCases() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr {
				return
			}

			// TestCases の比較 (Name, Input, Want を比較)
			if len(gotCases) != len(tt.wantCases) {
				t.Errorf("parseTestCases() len = %d, want %d", len(gotCases), len(tt.wantCases))
				return
			}
			for i := range gotCases {
				// Workspace のポインタが異なるため DeepEqual は使えない
				if gotCases[i].Name() != tt.wantCases[i].Name() {
					t.Errorf("Case %d: Name() = %q, want %q", i, gotCases[i].Name(), tt.wantCases[i].Name())
				}
				// Input() と Want() は内部でキャッシュを使うため、直接フィールドを比較
				if !bytes.Equal(gotCases[i].input, tt.wantCases[i].input) {
					t.Errorf("Case %d: input = %q, want %q", i, string(gotCases[i].input), string(tt.wantCases[i].input))
				}
				if !bytes.Equal(gotCases[i].want, tt.wantCases[i].want) {
					t.Errorf("Case %d: want = %q, want %q", i, string(gotCases[i].want), string(tt.wantCases[i].want))
				}
			}
		})
	}
}

func TestAtcoder_GetTestCases(t *testing.T) {
	contest := "abc100"
	problem := "a"
	targetPath := fmt.Sprintf("/contests/%s/tasks/%s_%s", contest, contest, problem)

	tests := []struct {
		name           string
		mockStatusCode int
		mockBody       string
		mockGetErr     error
		wantName       string // 最初のテストケースの名前
		wantErr        bool
	}{
		{
			name:           "正常系",
			mockStatusCode: http.StatusOK,
			mockBody: `
				<h3>入力例 1</h3><pre>1 2</pre>
				<h3>出力例 1</h3><pre>3</pre>
			`,
			wantName: "1",
			wantErr:  false,
		},
		{
			name:           "HTTPエラー",
			mockStatusCode: http.StatusNotFound,
			mockBody:       "Not Found",
			wantErr:        true,
		},
		{
			name:       "ネットワークエラー",
			mockGetErr: errors.New("network error"),
			wantErr:    true,
		},
		{
			name:           "パースエラー",
			mockStatusCode: http.StatusOK,
			mockBody:       `<h3>入力例 1</h3><pre>1 2</pre>`, // 不正なHTML
			wantErr:        true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a, mockTransport := newMockAtcoder(nil)

			mockTransport.RoundTripFunc = func(req *http.Request) (*http.Response, error) {
				if tt.mockGetErr != nil {
					return nil, tt.mockGetErr
				}
				if req.URL.Path != targetPath {
					return nil, fmt.Errorf("unexpected request path: %s, want %s", req.URL.Path, targetPath)
				}
				resp := &http.Response{
					StatusCode: tt.mockStatusCode,
					Body:       io.NopCloser(strings.NewReader(tt.mockBody)),
					Request:    req,
					Header:     make(http.Header),
				}
				return resp, nil
			}

			ts, err := a.GetTestCases(contest, problem)

			if (err != nil) != tt.wantErr {
				t.Errorf("GetTestCases() error = %v, wantErr %v", err, tt.wantErr)
			}
			if !tt.wantErr {
				if len(ts) == 0 {
					t.Errorf("GetTestCases() returned empty TestCases")
				} else if ts[0].Name() != tt.wantName {
					t.Errorf("GetTestCases() first case name = %q, want %q", ts[0].Name(), tt.wantName)
				}
			}
		})
	}
}

func TestAtcoder_parseProblemIDs(t *testing.T) {
	tests := []struct {
		name    string
		html    string
		wantIDs []string
		wantErr bool
	}{
		{
			name: "正常系: 複数ID、重複あり、大文字小文字混在",
			html: `
				<a href="/contests/abc100/tasks/abc100_a">Problem A</a>
				<a href="/contests/abc100/tasks/abc100_b">Problem B</a>
				<a href="/contests/abc100/tasks/abc100_A">Duplicate A</a>
				<a href="/contests/arc100/tasks/arc100_c">Another Contest</a>
				<a href="/contests/abc100/tasks/abc100_c">Problem C</a>
			`,
			wantIDs: []string{"a", "b", "c"}, // 小文字に変換され、重複除去される
			wantErr: false,
		},
		{
			name:    "正常系: IDが見つからない",
			html:    `<html><body>No links</body></html>`,
			wantIDs: nil, // 空スライスではなく nil を期待
			wantErr: false,
		},
		{
			name:    "正常系: HTMLが空",
			html:    "",
			wantIDs: nil,
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a, _ := newMockAtcoder(nil)
			reader := strings.NewReader(tt.html)

			gotIDs, err := a.parseProblemIDs(reader)

			if (err != nil) != tt.wantErr {
				t.Errorf("parseProblemIDs() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr {
				return
			}
			if !reflect.DeepEqual(gotIDs, tt.wantIDs) {
				t.Errorf("parseProblemIDs() = %v, want %v", gotIDs, tt.wantIDs)
			}
		})
	}
}

func TestAtcoder_GetProblemIDs(t *testing.T) {
	contest := "abc100"
	targetPath := fmt.Sprintf("/contests/%s/tasks", contest)

	tests := []struct {
		name           string
		mockStatusCode int
		mockBody       string
		mockGetErr     error
		wantIDs        []string
		wantErr        bool
	}{
		{
			name:           "正常系",
			mockStatusCode: http.StatusOK,
			mockBody:       `<a href="/contests/abc100/tasks/abc100_a">A</a> <a href="/contests/abc100/tasks/abc100_b">B</a>`,
			wantIDs:        []string{"a", "b"},
			wantErr:        false,
		},
		{
			name:           "HTTPエラー",
			mockStatusCode: http.StatusNotFound,
			wantErr:        true,
		},
		{
			name:       "ネットワークエラー",
			mockGetErr: errors.New("network error"),
			wantErr:    true,
		},
		{
			name:           "パースエラー (io.ReadAll エラー)",
			mockStatusCode: http.StatusOK,
			// Body をエラーを返す Reader にする
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a, mockTransport := newMockAtcoder(nil)

			mockTransport.RoundTripFunc = func(req *http.Request) (*http.Response, error) {
				if tt.mockGetErr != nil {
					return nil, tt.mockGetErr
				}
				if req.URL.Path != targetPath {
					return nil, fmt.Errorf("unexpected request path: %s, want %s", req.URL.Path, targetPath)
				}

				var body io.ReadCloser
				if tt.name == "パースエラー (io.ReadAll エラー)" {
					body = io.NopCloser(&errorReader{err: errors.New("read body failed")})
				} else {
					body = io.NopCloser(strings.NewReader(tt.mockBody))
				}

				resp := &http.Response{
					StatusCode: tt.mockStatusCode,
					Body:       body,
					Request:    req,
					Header:     make(http.Header),
				}
				return resp, nil
			}

			gotIDs, err := a.GetProblemIDs(contest)

			if (err != nil) != tt.wantErr {
				t.Errorf("GetProblemIDs() error = %v, wantErr %v", err, tt.wantErr)
			}
			if !tt.wantErr {
				if !reflect.DeepEqual(gotIDs, tt.wantIDs) {
					t.Errorf("GetProblemIDs() = %v, want %v", gotIDs, tt.wantIDs)
				}
			}
		})
	}
}

// errorReader は workspace_test.go にもあるが、ここにも定義 (テストファイル間の依存を避けるため)
// type errorReader struct {
// 	err error
// }
// func (r *errorReader) Read(p []byte) (n int, err error) {
// 	return 0, r.err
// }

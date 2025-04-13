package main

import (
	"bufio"
	"fmt"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"os"
	"path/filepath"
	"strings"
)

func loadCookies() ([]*http.Cookie, error) {
	path := cookiePath()
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return nil, nil
	}

	data, err := os.ReadFile(path)
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
		cookies = append(cookies, &http.Cookie{
			Name:  parts[0],
			Value: parts[1],
		})
	}
	return cookies, nil
}

func cookiePath() string {
	xdg := os.Getenv("XDG_DATA_HOME")
	if xdg == "" {
		home, _ := os.UserHomeDir()
		xdg = filepath.Join(home, ".local", "share")
	}
	return filepath.Join(xdg, "atcoder-cli", "cookie.txt")
}

func saveRawCookie(c string) error {
	path := cookiePath()
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		return err
	}
	return os.WriteFile(path, []byte(c+"\n"), 0600)
}

func promptCookie() (string, error) {
	fmt.Fprint(os.Stderr, "REVEL_SESSION cookie を入力してください: ")

	reader := bufio.NewReader(os.Stdin)
	cookie, err := reader.ReadString('\n')
	if err != nil {
		return "", err
	}
	cookie = strings.TrimSpace(cookie)

	return "REVEL_SESSION=" + cookie, nil
}

func newHttpClient() (*http.Client, error) {
	jar, err := cookiejar.New(nil)
	if err != nil {
		return nil, err
	}
	cookies, err := loadCookies()
	if err != nil {
		return nil, err
	}
	if cookies != nil {
		jar.SetCookies(&url.URL{Scheme: "https", Host: "atcoder.jp"}, cookies)
	}
	return &http.Client{Jar: jar}, nil
}

type loginCmdRunner struct {
	Command
	client *http.Client
}

func loginCheck(client *http.Client) (bool, error) {
	var url = "https://atcoder.jp/contests/abc001/submit"

	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return false, err
	}

	// リダイレクトを無効化
	client.CheckRedirect = func(req *http.Request, via []*http.Request) error {
		return http.ErrUseLastResponse
	}

	resp, err := client.Do(req)
	if err != nil {
		return false, err
	}
	defer resp.Body.Close()

	// 302 → リダイレクトされてる → 未ログイン
	// 200 → ログイン済
	if resp.StatusCode == http.StatusOK {
		return true, nil
	}
	if resp.StatusCode == http.StatusFound {
		return false, nil
	}
	return false, fmt.Errorf("予期しないレスポンスコード: %d", resp.StatusCode)
}

func NewLoginCmdRunner(cmd *Command) *loginCmdRunner {
	client, err := newHttpClient()
	if err != nil {
		panic(err)
	}
	return &loginCmdRunner{
		Command: *cmd,
		client:  client,
		// atcoderUrl: "https://atcoder.jp",
	}
}

func (c *loginCmdRunner) run() ExitCode {
	isLogin, err := loginCheck(c.client)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}
	if isLogin {
		fmt.Fprintln(os.Stderr, "既にログインしています")
		return ExitOK
	}

	cookie, err := promptCookie()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}

	if err := saveRawCookie("REVEL_SESSION=" + cookie); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}

	client, err := newHttpClient()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}

	isLogin, err = loginCheck(client)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return ExitError
	}

	if !isLogin {
		fmt.Fprintln(os.Stderr, "ログインに失敗しました")
		return ExitError
	}

	fmt.Fprintln(os.Stderr, "ログインに成功しました")

	return ExitOK
}

var loginCmd = &Command{
	Usage: "login",
	Short: "Atcoderのログイン情報をローカルに保存します",
	Long: `以下の手順に従いAtcoderのログイン情報(REVEL_SESSION)をローカル($HOME/.local/share/atcoder-cli/cookie.txt)に保存します。
	1. このコマンドを実行します。
	2. ブラウザでAtcoderにログインします。
	3. REVEL_SESSIONをコピーして貼り付けます。`,
	Aliases: []string{"l"},
	Run: func(cmd *Command, args []string) ExitCode {
		return NewLoginCmdRunner(cmd).run()
	},
}

func init() {
	cmd.AddCommand(loginCmd)
}

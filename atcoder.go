package main

import (
	"fmt"
	"io"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"regexp"
	"strconv"
	"strings"
)

type Atcoder struct {
	url    *url.URL
	client *http.Client
	w      *Workspace
}

func (a *Atcoder) ReloadCookies() error {
	cookies, err := a.w.LoadCookies()
	if err != nil {
		return err
	}
	if cookies != nil {
		a.client.Jar.SetCookies(a.url, cookies)
	}
	return nil
}

func (a *Atcoder) Get(path string) (*http.Response, error) {
	w := a.w
	w.Debugf("Call Get to %s", path)
	url := a.url.String() + path
	req, err := http.NewRequest("GET", url, nil)

	if err != nil {
		return nil, err
	}
	for _, c := range a.client.Jar.Cookies(req.URL) {
		w.Debugf("Sending Cookie: %s=%s", c.Name, c.Value)
	}

	resp, err := a.client.Do(req)
	if err != nil {
		return nil, err
	}

	w.Debugf("Response Status: %s", resp.Status)
	w.Debugf("Response URL: %s", resp.Request.URL)
	w.Debugf("Response Cookies: %v", resp.Cookies())
	w.Debugf("Response Headers: %v", resp.Header)
	w.Debugf("Response Body: %v", resp.Body)

	return resp, nil
}

func (a *Atcoder) Post(path string, data url.Values) (*http.Response, error) {
	url := a.url.String() + path
	req, err := http.NewRequest("POST", url, strings.NewReader(data.Encode()))

	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	return a.client.Do(req)
}

func (a *Atcoder) LoginCheck() (bool, error) {
	w := a.w
	w.Debug("Call LoginCheck")

	path := "/contests/abc001/submit"

	resp, err := a.Get(path)
	if err != nil {
		w.Debugf("Error when sending GET Request to %s: %v", path, err)
		return false, err
	}
	defer resp.Body.Close()

	if resp.Request.URL.Path != path {
		w.Debugf("Response Url.Path is not %s", path)
		w.Debug("Call LoginCheck done")
		return false, nil
	}

	if resp.StatusCode != 200 {
		w.Debug("Response StatusCode is not 200")
		w.Debug("Call LoginCheck done")
		return false, fmt.Errorf("HTTPエラー: %s", resp.Status)
	}

	w.Debug("Call LoginCheck done")
	return true, nil
}

func (a *Atcoder) GetProblemPage(contest string, problem string) (*http.Response, error) {
	path := fmt.Sprintf("/contests/%s/tasks/%s_%s", contest, strings.ReplaceAll(contest, "-", "_"), problem)
	return a.Get(path)
}

func (a *Atcoder) GetTestCases(contest string, problem string) (TestCases, error) {
	resp, err := a.GetProblemPage(contest, problem)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("HTTPエラー at GetTestCases: %s", resp.Status)
	}

	return a.parseTestCases(resp.Body)
}

func (a *Atcoder) parseTestCases(r io.Reader) (TestCases, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("読み込み失敗: %w", err)
	}
	html := string(data)

	// 入力例・出力例それぞれに続く <pre> の内容を抽出
	re := regexp.MustCompile(`<h3>(入力例|出力例)\s*\d+</h3>\s*<pre>([\s\S]*?)</pre>`)
	matches := re.FindAllStringSubmatch(html, -1)

	if len(matches)%2 != 0 {
		return nil, fmt.Errorf("入力と出力のペアが揃っていません")
	}

	var ts TestCases
	for i := 0; i+1 < len(matches); i += 2 {
		if matches[i][1] != "入力例" || matches[i+1][1] != "出力例" {
			return nil, fmt.Errorf("入力/出力の順番が想定と異なります")
		}
		input := strings.TrimSpace(matches[i][2])
		want := strings.TrimSpace(matches[i+1][2])
		t := NewTestCase(a.w)
		t.SetName(strconv.Itoa(i/2 + 1))
		t.SetInput([]byte(input))
		t.SetWant([]byte(want))
		ts.Add(t)
	}
	return ts, nil
}

func (a *Atcoder) GetTaskPage(contest string) (*http.Response, error) {
	path := fmt.Sprintf("/contests/%s/tasks", contest)
	return a.Get(path)
}

func (a *Atcoder) GetProblemIDs(contest string) ([]string, error) {
	resp, err := a.GetTaskPage(contest)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("HTTPエラー at GetProblemIDs: %s", resp.Status)
	}

	return a.parseProblemIDs(resp.Body)
}

func (a *Atcoder) parseProblemIDs(r io.Reader) ([]string, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("読み込み失敗: %w", err)
	}
	html := string(data)

	re := regexp.MustCompile(`/contests/[^/]+/tasks/[^/]+_([a-zA-Z0-9]+)`)
	matches := re.FindAllStringSubmatch(html, -1)

	seen := make(map[string]bool)
	var ids []string
	for _, m := range matches {
		id := strings.ToLower(m[1])
		if !seen[id] {
			ids = append(ids, id)
			seen[id] = true
		}
	}
	return ids, nil
}

func newHttpClient(w *Workspace, url *url.URL) (*http.Client, error) {
	jar, err := cookiejar.New(nil)
	if err != nil {
		return nil, err
	}
	cookies, err := w.LoadCookies()
	if err != nil {
		return nil, err
	}
	if cookies != nil {
		jar.SetCookies(url, cookies)
	}
	return &http.Client{Jar: jar}, nil
}

func NewAtcoder(w *Workspace) *Atcoder {
	var url = &url.URL{Scheme: "https", Host: "atcoder.jp"}
	client, err := newHttpClient(w, url)
	if err != nil {
		panic(err)
	}
	return &Atcoder{
		url:    url,
		client: client,
		w:      w,
	}
}

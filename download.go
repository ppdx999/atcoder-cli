package main

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

var downloadCmd = &Command{
	Usage:   "download",
	Short:   "問題のサンプルケースをダウンロードします",
	Aliases: []string{"d"},
	Run: func(cmd *Command, args []string) ExitCode {
		return runDownload()
	},
}

func init() {
	cmd.AddCommand(downloadCmd)
}

func runDownload() ExitCode {
	config, err := getConfig()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fail to get config: %v\n", err)
		return ExitError
	}

	samples, err := fetchSamples(config)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Fail to get samples: %v\n", err)
		return ExitError
	}

	if err := writeSamples(samples); err != nil {
		fmt.Fprintf(os.Stderr, "Fail to write samples: %v\n", err)
		return ExitError
	}
	return ExitOK
}

type config struct {
	contest string
	problem string
}

func getConfig() (c config, err error) {
	cwd, err := os.Getwd()
	if err != nil {
		return
	}
	c.problem = filepath.Base(cwd)
	c.contest = filepath.Base(filepath.Dir(cwd))
	return
}

type sample struct {
	in  string
	out string
}

func (s sample) String() string {
	return fmt.Sprintf("in: %s, out: %s", s.in, s.out)
}

func fetchSamples(c config) ([]sample, error) {
	body, err := fetchProblemPage(c)
	if err != nil {
		return nil, err
	}
	return parseSamples(body)
}

func fetchProblemPage(c config) (io.Reader, error) {
	url := fmt.Sprintf("https://atcoder.jp/contests/%s/tasks/%s_%s", c.contest, strings.ReplaceAll(c.contest, "-", "_"), c.problem)

	resp, err := http.Get(url)
	if err != nil {
		return nil, fmt.Errorf("HTTPリクエスト失敗: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("HTTPエラー: %s", resp.Status)
	}

	buf, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	return bytes.NewReader(buf), nil
}

func parseSamples(r io.Reader) ([]sample, error) {
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

	var samples []sample
	for i := 0; i+1 < len(matches); i += 2 {
		if matches[i][1] != "入力例" || matches[i+1][1] != "出力例" {
			return nil, fmt.Errorf("入力/出力の順番が想定と異なります")
		}
		in := strings.TrimSpace(matches[i][2])
		out := strings.TrimSpace(matches[i+1][2])
		samples = append(samples, sample{in, out})
	}
	return samples, nil
}

func writeSamples(samples []sample) error {
	testDir := "test"

	if err := os.MkdirAll(testDir, 0755); err != nil {
		return fmt.Errorf("テストディレクトリ作成失敗: %w", err)
	}

	for i, s := range samples {
		inPath := filepath.Join(testDir, fmt.Sprintf("%d.in", i))
		outPath := filepath.Join(testDir, fmt.Sprintf("%d.out", i))

		if err := os.WriteFile(inPath, []byte(s.in), 0644); err != nil {
			return fmt.Errorf("入力ファイル書き込み失敗: %w", err)
		}
		if err := os.WriteFile(outPath, []byte(s.out), 0644); err != nil {
			return fmt.Errorf("出力ファイル書き込み失敗: %w", err)
		}
	}

	return nil
}

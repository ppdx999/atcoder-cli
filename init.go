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

// initCmd represents the init command
var initCmd = &Command{
	Usage:   "init <contest>",
	Short:   "コンテストのディレクトリ構造を初期化します",
	Aliases: []string{"i"},
	Run: func(cmd *Command, args []string) ExitCode {
		logger := cmd.Logger
		logger.Debug("Execute init")
		if len(args) == 0 {
			fmt.Fprintln(os.Stderr, "コンテスト名を指定してください")
			return ExitError
		}
		return runInit(args[0])
	},
}

func init() {
	cmd.AddCommand(initCmd)
}

func runInit(contest string) ExitCode {
	problems, err := problemIDs(contest)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fail to get problem ids: %v\n", err)
		return ExitError
	}

	if err := os.Mkdir(contest, 0755); err != nil && !os.IsExist(err) {
		fmt.Fprintf(os.Stderr, "Fail to create contest directory, %s : %v\n", contest, err)
		return ExitError
	}

	for _, p := range problems {
		path := filepath.Join(contest, p)
		if err := os.Mkdir(path, 0755); err != nil && !os.IsExist(err) {
			fmt.Fprintf(os.Stderr, "Fail to create problem directory, %s : %v\n", p, err)
			return ExitError
		}
		fmt.Println("Created:", path)
	}
	return ExitOK
}

func problemIDs(contest string) ([]string, error) {
	body, err := fetchTasksPage(contest)
	if err != nil {
		return nil, err
	}
	return parseProblemIDs(body)
}

func fetchTasksPage(contest string) (io.Reader, error) {
	url := fmt.Sprintf("https://atcoder.jp/contests/%s/tasks", contest)
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

func parseProblemIDs(r io.Reader) ([]string, error) {
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

package main

import "os"

var cmd = &Command{
	Usage: "atcoder-cli",
	Short: "atcoderを便利に使う小さなコマンド群",
}

func main() {
	cmd.Execute(os.Args[1:])
}

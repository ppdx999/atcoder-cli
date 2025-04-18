package main

import "os"

var cmd = NewCommand(
	&NewCommandOpt[any]{
		Usage: "atcoder-cli",
		Short: "atcoderを便利に使う小さなコマンド群",
	},
)

func main() {
	cmd.SetWorkspace(ProdWorkspace)
	os.Exit(int(cmd.Execute(os.Args[1:])))
}

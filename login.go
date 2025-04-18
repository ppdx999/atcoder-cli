package main

type LoginCmdRunner struct {
	w       *Workspace
	atcoder *Atcoder
}

func (c *LoginCmdRunner) Setup(cmd *Command[LoginCmdOpt]) error {
	w := cmd.Workspace()
	c.w = w
	c.atcoder = NewAtcoder(w)
	return nil
}

func (c *LoginCmdRunner) Run(opt LoginCmdOpt) ExitCode {
	isLogin, err := c.atcoder.LoginCheck()
	if err != nil {
		c.w.Error(err.Error())
		return ExitError
	}

	if isLogin {
		c.w.Error("既にログインしています")
		return ExitOK
	}

	cookie, err := c.w.PromptCookie()
	if err != nil {
		c.w.Error(err.Error())
		return ExitError
	}

	if err := c.w.SaveCookie(cookie); err != nil {
		c.w.Error(err.Error())
		return ExitError
	}

	err = c.atcoder.ReloadCookies()
	if err != nil {
		c.w.Error(err.Error())
		return ExitError
	}

	isLogin, err = c.atcoder.LoginCheck()
	if err != nil {
		c.w.Error(err.Error())
		return ExitError
	}

	if !isLogin {
		c.w.Error("ログインに失敗しました")
		return ExitError
	}

	c.w.Error("ログインに成功しました")

	return ExitOK
}

type LoginCmdOpt struct{}

var loginCmd = NewCommand(
	&NewCommandOpt[LoginCmdOpt]{
		Usage: "login",
		Short: "Atcoderのログイン情報をローカルに保存します",
		Long: `以下の手順に従いAtcoderのログイン情報(REVEL_SESSION)をローカル($HOME/.local/share/atcoder-cli/cookie.txt)に保存します。
				1. このコマンドを実行します。
				2. ブラウザでAtcoderにログインします。
				3. REVEL_SESSIONをコピーして貼り付けます。`,
		Aliases: []string{"l"},
		Runner:  &LoginCmdRunner{},
	},
)

func init() {
	cmd.Add(loginCmd)
}

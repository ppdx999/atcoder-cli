package main

import (
	"bytes"
	"flag"
	"fmt"
	"strings"
	"text/template"
)

type ExitCode int

const (
	ExitOK    ExitCode = 0
	ExitError ExitCode = 1
)

type BaseCommand struct {
	Usage         string
	Short         string
	Long          string
	Aliases       []string
	RunBase       func(args []string) ExitCode
	parent        *BaseCommand
	children      []*BaseCommand
	runnable      bool
	isHelpMode    bool
	isVerboseMode bool
	_w            *Workspace
	_flags        *flag.FlagSet
}

func (c *BaseCommand) name() string {
	name := c.Usage
	i := strings.Index(name, " ")
	if i >= 0 {
		name = name[:i]
	}
	return name
}

func (c *BaseCommand) hasAlias(s string) bool {
	for _, a := range c.Aliases {
		if a == s {
			return true
		}
	}
	return false
}

func (c *BaseCommand) findNext(next string) *BaseCommand {
	for _, cmd := range c.children {
		if cmd.name() == next || cmd.hasAlias(next) {
			return cmd
		}
	}
	return nil
}

func (c *BaseCommand) traverse(args []string) (*BaseCommand, []string) {
	if len(args) == 0 {
		return c, args
	}

	cmd := c.findNext(args[0])
	if cmd == nil {
		return c, args
	}

	return cmd.traverse(args[1:])
}

func (c *BaseCommand) hasParent() bool {
	return c.parent != nil
}

func (c *BaseCommand) root() *BaseCommand {
	if c.hasParent() {
		return c.parent.root()
	}
	return c
}

func (c *BaseCommand) hasSubCommands() bool {
	return len(c.children) > 0
}

func (c *BaseCommand) commandPath() string {
	if c.hasParent() {
		return c.parent.commandPath() + " " + c.name()
	}
	return c.name()
}

func (c *BaseCommand) nameAndAliases() string {
	return strings.Join(append([]string{c.name()}, c.Aliases...), ", ")
}

func (c *BaseCommand) description() string {
	d := strings.TrimSpace(c.Long)
	if d == "" {
		d = c.Short
	}
	return d
}

func (c *BaseCommand) hasDescription() bool {
	return c.Long != "" || c.Short != ""
}

func (c *BaseCommand) usageLine() string {
	if c.hasParent() {
		return c.parent.usageLine() + " " + c.Usage
	}
	return c.Usage
}

func (c *BaseCommand) flagUsage() string {
	var buf bytes.Buffer
	f := c.flags()
	f.VisitAll(func(f *flag.Flag) {
		fmt.Fprintf(&buf, "  -%-10s\t%s\n", f.Name, f.Usage)
	})
	return buf.String()
}

const usageTemplate = `Usage:
{{- if .Runnable }}
  {{ .UsageLine }}
{{- end }}
{{- if .HasSubCommands }}
  {{ .CommandPath }} [command]

Available Commands:
{{- range .SubCommands }}
  {{ printf "%-16s %s" .Name .Desc }}
{{- end }}
{{- end }}
{{- if .HasAliases }}

Aliases:
  {{ .AliasesLine }}
{{- end }}
{{- if .HasDescription }}

Description:
  {{ .Description }}
{{- end }}
{{- if .HasFlags }}

Flags:
{{ .FlagUsage }}
{{- end }}
`

type cmdOverview struct {
	Name string
	Desc string
}

func (c *BaseCommand) subcommandOverview() []cmdOverview {
	var overview []cmdOverview
	for _, cmd := range c.children {
		overview = append(overview, cmdOverview{
			Name: cmd.name(),
			Desc: cmd.Short,
		})
	}
	return overview
}

func (c *BaseCommand) usage() ExitCode {
	w := c.workspace()
	tmpl := template.Must(template.New("usage").Parse(usageTemplate))
	err := tmpl.Execute(w.GetErr(), map[string]any{
		"Runnable":       c.runnable,
		"UsageLine":      c.usageLine(),
		"HasSubCommands": c.hasSubCommands(),
		"CommandPath":    c.commandPath(),
		"SubCommands":    c.subcommandOverview(),
		"HasAliases":     len(c.Aliases) > 0,
		"AliasesLine":    c.nameAndAliases(),
		"HasDescription": c.hasDescription(),
		"Description":    c.description(),
		"HasFlags":       c.flags() != nil,
		"FlagUsage":      c.flagUsage(),
	})
	if err != nil {
		w.Errorf("Error rendering usage: %v", err)
	}
	return ExitError
}

func (c *BaseCommand) setupCommonFlags() {
	f := c.flags()
	f.BoolVar(&c.isHelpMode, "h", false, "ヘルプを表示します")
	f.BoolVar(&c.isHelpMode, "help", false, "ヘルプを表示します")
	f.BoolVar(&c.isVerboseMode, "v", false, "詳細なログを出力します")
	f.BoolVar(&c.isVerboseMode, "verbose", false, "詳細なログを出力します")
}

func (c *BaseCommand) addBaseCommand(cmd *BaseCommand) {
	cmd.parent = c
	c.children = append(c.children, cmd)
}

func (c *BaseCommand) flags() *flag.FlagSet {
	if c._flags == nil {
		c._flags = flag.NewFlagSet(c.name(), flag.ContinueOnError)
	}
	return c._flags
}

func (c *BaseCommand) workspace() *Workspace {
	if c._w != nil {
		return c._w
	}
	w := c.root().workspace()
	if w == nil {
		panic("workspace is nil")
	}
	c._w = w
	return w
}

func (c *BaseCommand) setWorkspace(w *Workspace) {
	c._w = w
}

func (c *BaseCommand) execute(args []string) ExitCode {
	if c.hasParent() {
		c.workspace().Error("Execute Non Root command")
		return ExitError
	}

	cmd, args := c.traverse(args)

	return cmd.RunBase(args)
}

type CmdRunner[T any] interface {
	Setup(cmd *Command[T]) error
	Run(opt T) ExitCode
}

type Command[T any] struct {
	base     *BaseCommand
	Opt      T
	ParseOpt func(args []string) (T, error)
	Runner   CmdRunner[T]
}

func (c *Command[T]) Workspace() *Workspace {
	return c.base.workspace()
}

func (c *Command[T]) SetWorkspace(w *Workspace) {
	c.base.setWorkspace(w)
}

func (c *Command[T]) run(args []string) ExitCode {
	w := c.Workspace()
	base := c.base
	base.setupCommonFlags()

	flag := base.flags()

	if err := flag.Parse(args); err != nil {
		w.Error(err.Error())
		return ExitError
	}

	if !base.runnable || base.isHelpMode {
		return base.usage()
	}

	if base.isVerboseMode {
		w.SetLogLevel(LogLevelDebug)
	} else {
		w.SetLogLevel(LogLevelInfo)
	}

	if c.ParseOpt != nil {
		opt, err := c.ParseOpt(flag.Args())
		if err != nil {
			w.Error(err.Error())
			return ExitError
		}
		c.Opt = opt
	}

	if c.Runner == nil {
		w.Error("Runner is nil")
	}
	if err := c.Runner.Setup(c); err != nil {
		w.Error(err.Error())
		return ExitError
	}

	return c.Runner.Run(c.Opt)
}

func (c *Command[T]) Base() *BaseCommand {
	return c.base
}

type CommandAddArg interface {
	Base() *BaseCommand
}

func (c *Command[T]) Add(cmd CommandAddArg) {
	c.base.addBaseCommand(cmd.Base())
}

func (c *Command[T]) Execute(args []string) ExitCode {
	return c.base.execute(args)
}

type NewCommandOpt[T any] struct {
	Usage    string
	Short    string
	Long     string
	Aliases  []string
	ParseOpt func(args []string) (T, error)
	Runner   CmdRunner[T]
}

func NewCommand[T any](opt *NewCommandOpt[T]) *Command[T] {
	base := &BaseCommand{
		Usage:    opt.Usage,
		Short:    opt.Short,
		Long:     opt.Long,
		Aliases:  opt.Aliases,
		runnable: opt.Runner != nil,
		RunBase:  nil,
	}

	cmd := &Command[T]{
		base:     base,
		ParseOpt: opt.ParseOpt,
		Runner:   opt.Runner,
	}

	base.RunBase = cmd.run

	return cmd
}

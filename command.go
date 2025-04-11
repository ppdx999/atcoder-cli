package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"log/slog"
	"os"
	"strings"
	"text/template"
)

type ExitCode int

const (
	ExitOK    ExitCode = 0
	ExitError ExitCode = 1
)

type Command struct {
	Usage         string
	Short         string
	Long          string
	Aliases       []string
	Run           func(cmd *Command, args []string) ExitCode
	Logger        *slog.Logger
	parent        *Command
	commands      []*Command
	flags         *flag.FlagSet
	isHelpMode    bool
	isVerboseMode bool
}

func (c *Command) name() string {
	name := c.Usage
	i := strings.Index(name, " ")
	if i >= 0 {
		name = name[:i]
	}
	return name
}

func (c *Command) hasAlias(s string) bool {
	for _, a := range c.Aliases {
		if a == s {
			return true
		}
	}
	return false
}

func (c *Command) findNext(next string) *Command {
	for _, cmd := range c.commands {
		if cmd.name() == next || cmd.hasAlias(next) {
			return cmd
		}
	}
	return nil
}

func (c *Command) traverse(args []string) (*Command, []string) {
	if len(args) == 0 {
		return c, args
	}

	cmd := c.findNext(args[0])
	if cmd == nil {
		return c, args
	}

	return cmd.traverse(args[1:])
}

func (c *Command) hasParent() bool {
	return c.parent != nil
}

func (c *Command) runnable() bool {
	return c.Run != nil
}

func (c *Command) hasSubCommands() bool {
	return len(c.commands) > 0
}

func (c *Command) commandPath() string {
	if c.hasParent() {
		return c.parent.commandPath() + " " + c.name()
	}
	return c.name()
}

func (c *Command) nameAndAliases() string {
	return strings.Join(append([]string{c.name()}, c.Aliases...), ", ")
}

func (c *Command) description() string {
	d := strings.TrimSpace(c.Long)
	if d == "" {
		d = c.Short
	}
	return d
}

func (c *Command) hasDescription() bool {
	return c.Long != "" || c.Short != ""
}

func (c *Command) usageLine() string {
	if c.hasParent() {
		return c.parent.usageLine() + " " + c.Usage
	}
	return c.Usage
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

func (c *Command) subcommandOverview() []cmdOverview {
	var overview []cmdOverview
	for _, cmd := range c.commands {
		overview = append(overview, cmdOverview{
			Name: cmd.name(),
			Desc: cmd.Short,
		})
	}
	return overview
}

func (c *Command) usage(w io.Writer) ExitCode {
	tmpl := template.Must(template.New("usage").Parse(usageTemplate))
	err := tmpl.Execute(w, map[string]any{
		"Runnable":       c.runnable(),
		"UsageLine":      c.usageLine(),
		"HasSubCommands": c.hasSubCommands(),
		"CommandPath":    c.commandPath(),
		"SubCommands":    c.subcommandOverview(),
		"HasAliases":     len(c.Aliases) > 0,
		"AliasesLine":    c.nameAndAliases(),
		"HasDescription": c.hasDescription(),
		"Description":    c.description(),
		"HasFlags":       c.Flags() != nil,
		"FlagUsage":      c.flagUsage(),
	})
	if err != nil {
		fmt.Fprintf(w, "Error rendering usage: %v\n", err)
	}
	return ExitError
}

func (c *Command) setupCommonFlags() {
	c.Flags().BoolVar(&c.isHelpMode, "h", false, "ヘルプを表示します")
	c.Flags().BoolVar(&c.isHelpMode, "help", false, "ヘルプを表示します")
	c.Flags().BoolVar(&c.isVerboseMode, "v", false, "詳細なログを出力します")
	c.Flags().BoolVar(&c.isVerboseMode, "verbose", false, "詳細なログを出力します")
}

func (c *Command) flagUsage() string {
	var buf bytes.Buffer
	flagSet := c.Flags()
	flagSet.VisitAll(func(f *flag.Flag) {
		fmt.Fprintf(&buf, "  -%-10s\t%s\n", f.Name, f.Usage)
	})
	return buf.String()
}

func (c *Command) setupLogger(logLevel slog.Level) {
	if c.Logger == nil {
		c.Logger = slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: logLevel}))
	}
}

func (c *Command) AddCommand(cmd *Command) {
	cmd.parent = c
	c.commands = append(c.commands, cmd)
}

func (c *Command) Flags() *flag.FlagSet {
	if c.flags == nil {
		c.flags = flag.NewFlagSet(c.name(), flag.ContinueOnError)
	}
	return c.flags
}

func (c *Command) Execute(args []string) ExitCode {
	if c.hasParent() {
		fmt.Fprintf(os.Stderr, "Execute non root command")
		return ExitError
	}

	cmd, args := c.traverse(args)

	cmd.setupCommonFlags()
	cmd.flags.Parse(args)
	args = cmd.flags.Args()

	if !cmd.runnable() || cmd.isHelpMode {
		return cmd.usage(os.Stderr)
	}

	logLevel := slog.LevelInfo
	if cmd.isVerboseMode {
		logLevel = slog.LevelDebug
	}
	cmd.setupLogger(logLevel)

	return cmd.Run(cmd, args)
}

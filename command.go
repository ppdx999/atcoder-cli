package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

type ExitCode int

const (
	ExitOK    ExitCode = 0
	ExitError ExitCode = 1
)

func rpad(s string, padStr string, pLen int) string {
	return s + strings.Repeat(padStr, pLen-len(s))
}

type Command struct {
	Usage    string
	Short    string
	Long     string
	Aliases  []string
	Run      func(cmd *Command, args []string) ExitCode
	parent   *Command
	commands []*Command
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

func (c *Command) maxSubcommandNameLen() int {
	maxLen := 0
	for _, cmd := range c.commands {
		if len(cmd.name()) > maxLen {
			maxLen = len(cmd.name())
		}
	}
	return maxLen
}

func (c *Command) hasDescription() bool {
	return c.Long != "" || c.Short != ""
}

func (c *Command) isHelp(args []string) bool {
	return len(args) == 1 && (args[0] == "-h" || args[0] == "--help")
}

func (c *Command) usageLine() string {
	if c.hasParent() {
		return c.parent.usageLine() + " " + c.Usage
	}
	return c.Usage
}

func (c *Command) usage(w io.Writer) ExitCode {
	fmt.Fprint(w, "Usage:")
	if c.runnable() {
		fmt.Fprintf(w, "\n  %s", c.usageLine())
	}
	if c.hasSubCommands() {
		fmt.Fprintf(w, "\n  %s [command]", c.commandPath())
		fmt.Fprintf(w, "\n\nAvailable Commands:")
		for _, cmd := range c.commands {
			name := rpad(cmd.name(), " ", c.maxSubcommandNameLen()+2)
			fmt.Fprintf(w, "\n  %s %s", name, cmd.Short)
		}
	}
	if len(c.Aliases) > 0 {
		fmt.Fprintf(w, "\n\nAliases:")
		fmt.Fprintf(w, "\n  %s", c.nameAndAliases())
	}
	if c.hasDescription() {
		fmt.Fprintf(w, "\n\nDescription:")
		fmt.Fprintf(w, "\n  %s", c.description())
	}
	return ExitError
}

func (c *Command) AddCommand(cmd *Command) {
	cmd.parent = c
	c.commands = append(c.commands, cmd)
}

func (c *Command) Execute() ExitCode {
	if c.hasParent() {
		fmt.Fprintf(os.Stderr, "Execute non root command")
		return ExitError
	}

	cmd, args := c.traverse(os.Args[1:])

	if !cmd.runnable() || cmd.isHelp(args) {
		return cmd.usage(os.Stderr)
	}

	return cmd.Run(cmd, args)
}

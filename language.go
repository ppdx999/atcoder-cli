package main

type Language struct {
	Name       string
	MainFile   string
	BuildCmd   []string
	RunCmd     []string
	CleanupCmd []string
}

var Languages = []Language{
	{
		Name:       "go",
		MainFile:   "main.go",
		BuildCmd:   []string{"go", "build", "-o", "main", "main.go"},
		RunCmd:     []string{"./main"},
		CleanupCmd: []string{"rm", "main"},
	},
	{
		Name:       "cpp",
		MainFile:   "main.cpp",
		BuildCmd:   []string{"g++", "-o", "main", "main.cpp"},
		RunCmd:     []string{"./main"},
		CleanupCmd: []string{"rm", "main"},
	},
	{
		Name:     "python",
		MainFile: "main.py",
		RunCmd:   []string{"python3", "main.py"},
	},
	{
		Name:       "zig",
		MainFile:   "main.zig",
		BuildCmd:   []string{"zig", "build", "main.zig"},
		RunCmd:     []string{"./main"},
		CleanupCmd: []string{"rm", "main"},
	},
}

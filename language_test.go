package main

func NewMockLang() *Language {
	return &Language{
		Name:       "mock",
		MainFile:   "main.mock",
		BuildCmd:   []string{"mock", "build"},
		RunCmd:     []string{"./main_mock"},
		CleanupCmd: []string{"mock_rm", "main_mock"},
	}
}

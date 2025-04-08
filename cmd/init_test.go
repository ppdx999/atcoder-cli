package cmd

import (
	"strings"
	"testing"
)

func TestParseProblemIDs(t *testing.T) {
	tests := []struct {
		name     string
		html     string
		expected []string
	}{
		{
			name: "basic ABC",
			html: `
				<a href="/contests/abc123/tasks/abc123_a">A</a>
				<a href="/contests/abc123/tasks/abc123_b">B</a>
				<a href="/contests/abc123/tasks/abc123_c">C</a>
			`,
			expected: []string{"a", "b", "c"},
		},
		{
			name: "with underscore in contest name",
			html: `
				<a href="/contests/tessoku_book/tasks/tessoku_book_a">A</a>
				<a href="/contests/tessoku_book/tasks/tessoku_book_b">B</a>
			`,
			expected: []string{"a", "b"},
		},
		{
			name: "duplicate links",
			html: `
				<a href="/contests/abc/tasks/abc_a">A</a>
				<a href="/contests/abc/tasks/abc_a">A</a>
			`,
			expected: []string{"a"},
		},
		{
			name:     "no matching links",
			html:     `<p>No tasks here</p>`,
			expected: []string{},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := parseProblemIDs(strings.NewReader(tc.html))
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if len(got) != len(tc.expected) {
				t.Errorf("length mismatch: got %v, want %v", got, tc.expected)
			}
			for i := range got {
				if got[i] != tc.expected[i] {
					t.Errorf("mismatch at %d: got %q, want %q", i, got[i], tc.expected[i])
				}
			}
		})
	}
}

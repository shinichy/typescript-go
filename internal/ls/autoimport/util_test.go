package autoimport

import (
	"reflect"
	"testing"

	"github.com/microsoft/typescript-go/internal/vfs/vfstest"
	"gotest.tools/v3/assert"
)

func TestWordIndices(t *testing.T) {
	t.Parallel()
	tests := []struct {
		input         string
		expectedWords []string
	}{
		// Basic camelCase
		{
			input:         "camelCase",
			expectedWords: []string{"camelCase", "Case"},
		},
		// snake_case
		{
			input:         "snake_case",
			expectedWords: []string{"snake_case", "case"},
		},
		// ParseURL - uppercase sequence followed by lowercase
		{
			input:         "ParseURL",
			expectedWords: []string{"ParseURL", "URL"},
		},
		// XMLHttpRequest - multiple uppercase sequences
		{
			input:         "XMLHttpRequest",
			expectedWords: []string{"XMLHttpRequest", "HttpRequest", "Request"},
		},
		// Single word lowercase
		{
			input:         "hello",
			expectedWords: []string{"hello"},
		},
		// Single word uppercase
		{
			input:         "HELLO",
			expectedWords: []string{"HELLO"},
		},
		// Mixed with numbers
		{
			input:         "parseHTML5Parser",
			expectedWords: []string{"parseHTML5Parser", "HTML5Parser", "Parser"},
		},
		// Underscore variations
		{
			input:         "__proto__",
			expectedWords: []string{"__proto__", "proto__"},
		},
		{
			input:         "_private_member",
			expectedWords: []string{"_private_member", "member"},
		},
		// Single character
		{
			input:         "a",
			expectedWords: []string{"a"},
		},
		{
			input:         "A",
			expectedWords: []string{"A"},
		},
		// Consecutive underscores
		{
			input:         "test__double__underscore",
			expectedWords: []string{"test__double__underscore", "double__underscore", "underscore"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			t.Parallel()
			indices := wordIndices(tt.input)

			// Convert indices to actual word slices for comparison
			var actualWords []string
			for _, idx := range indices {
				actualWords = append(actualWords, tt.input[idx:])
			}

			if !reflect.DeepEqual(actualWords, tt.expectedWords) {
				t.Errorf("wordIndices(%q) produced words %v, want %v", tt.input, actualWords, tt.expectedWords)
			}
		})
	}
}

// TestGetPackageRealpathFuncs_FollowsNodeModulesSymlinks tests that toRealpath correctly
// follows symlinks for files outside the package directory (e.g. node_modules entries).
// Without this, the module resolver uses unresolved symlink paths as cache keys, causing
// the same file to be loaded multiple times and triggering massive memory usage when
// barrel files re-export from many symlinked packages (issue #2780).
func TestGetPackageRealpathFuncs_FollowsNodeModulesSymlinks(t *testing.T) {
	t.Parallel()

	// Simulate a layout where the package directory is itself a symlink (e.g. Bazel's
	// convenience symlinks or pnpm's virtual store):
	//   /symlink-bin/pkg/              -> symlink to /real/bin/pkg/
	//   /real/bin/pkg/node_modules/dep -> symlink to /real/dep/
	//
	// When toRealpath is used as the module resolver's Realpath, it must follow
	// the node_modules symlink so that /real/bin/pkg/node_modules/dep/index.d.ts
	// resolves to /real/dep/index.d.ts — otherwise the same dep file gets different
	// cache keys depending on which path it was reached through.
	fs := vfstest.FromMap(map[string]any{
		"/symlink-bin/pkg":                        vfstest.Symlink("/real/bin/pkg"),
		"/real/bin/pkg/index.d.ts":                "export declare const a: number;",
		"/real/bin/pkg/node_modules/dep":           vfstest.Symlink("/real/dep"),
		"/real/dep/index.d.ts":                    "export declare const b: number;",
	}, true)

	toRealpath, _ := getPackageRealpathFuncs(fs, "/symlink-bin/pkg")

	// Files inside the package should be converted via string replacement (fast path).
	assert.Equal(t,
		toRealpath("/symlink-bin/pkg/index.d.ts"),
		"/real/bin/pkg/index.d.ts",
		"package files should be converted via prefix replacement",
	)

	// Files outside the package (e.g. node_modules symlinks) should be resolved via
	// fs.Realpath so the cache key is the canonical realpath, not the symlink path.
	assert.Equal(t,
		toRealpath("/real/bin/pkg/node_modules/dep/index.d.ts"),
		"/real/dep/index.d.ts",
		"node_modules symlinks must be followed so the same file gets a consistent cache key",
	)
}

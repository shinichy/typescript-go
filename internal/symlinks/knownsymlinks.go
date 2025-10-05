package symlinks

import (
	"strings"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/collections"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/debug"
	"github.com/microsoft/typescript-go/internal/module"
	"github.com/microsoft/typescript-go/internal/tspath"
)

type KnownDirectoryLink struct {
	/**
	 * Matches the casing returned by `realpath`.  Used to compute the `realpath` of children.
	 * Always has trailing directory separator
	 */
	Real string
	/**
	 * toPath(real).  Stored to avoid repeated recomputation.
	 * Always has trailing directory separator
	 */
	RealPath tspath.Path
}

type KnownSymlinks struct {
	directories               collections.SyncMap[tspath.Path, *KnownDirectoryLink]
	directoriesByRealpath     collections.MultiMap[tspath.Path, string]
	files                     collections.SyncMap[tspath.Path, string]
	HasProcessedResolutions   bool
	cwd                       string
	useCaseSensitiveFileNames bool
}

/** Gets a map from symlink to realpath. Keys have trailing directory separators. */
func (cache *KnownSymlinks) Directories() *collections.SyncMap[tspath.Path, *KnownDirectoryLink] {
	return &cache.directories
}

func (cache *KnownSymlinks) DirectoriesByRealpath() *collections.MultiMap[tspath.Path, string] {
	return &cache.directoriesByRealpath
}

/** Gets a map from symlink to realpath */
func (cache *KnownSymlinks) Files() *collections.SyncMap[tspath.Path, string] {
	return &cache.files
}

// all callers should check !containsIgnoredPath(symlinkPath)
func (cache *KnownSymlinks) SetDirectory(symlink string, symlinkPath tspath.Path, realDirectory *KnownDirectoryLink) {
	// Large, interconnected dependency graphs in pnpm will have a huge number of symlinks
	// where both the realpath and the symlink path are inside node_modules/.pnpm. Since
	// this path is never a candidate for a module specifier, we can ignore it entirely.

	if realDirectory != nil {
		if _, ok := cache.directories.Load(symlinkPath); !ok {
			cache.directoriesByRealpath.Add(realDirectory.RealPath, symlink)
		}
	}
	cache.directories.Store(symlinkPath, realDirectory)
}

func (cache *KnownSymlinks) SetFile(symlinkPath tspath.Path, realpath string) {
	cache.files.Store(symlinkPath, realpath)
}

func NewKnownSymlink(currentDirectory string, useCaseSensitiveFileNames bool) *KnownSymlinks {
	return &KnownSymlinks{cwd: currentDirectory, useCaseSensitiveFileNames: useCaseSensitiveFileNames}
}

func (cache *KnownSymlinks) SetSymlinksFromResolutions(
	forEachResolvedModule func(callback func(resolution *module.ResolvedModule, moduleName string, mode core.ResolutionMode, filePath tspath.Path), file *ast.SourceFile),
	forEachResolvedTypeReferenceDirective func(callback func(resolution *module.ResolvedTypeReferenceDirective, moduleName string, mode core.ResolutionMode, filePath tspath.Path), file *ast.SourceFile),
) {
	debug.Assert(!cache.HasProcessedResolutions)
	cache.HasProcessedResolutions = true
	forEachResolvedModule(func(resolution *module.ResolvedModule, moduleName string, mode core.ResolutionMode, filePath tspath.Path) {
		cache.processResolution(resolution.OriginalPath, resolution.ResolvedFileName)
	}, nil)
	forEachResolvedTypeReferenceDirective(func(resolution *module.ResolvedTypeReferenceDirective, moduleName string, mode core.ResolutionMode, filePath tspath.Path) {
		cache.processResolution(resolution.OriginalPath, resolution.ResolvedFileName)
	}, nil)
}

func (cache *KnownSymlinks) processResolution(originalPath string, resolvedFileName string) {
	if originalPath == "" || resolvedFileName == "" {
		return
	}
	cache.SetFile(tspath.ToPath(originalPath, cache.cwd, cache.useCaseSensitiveFileNames), resolvedFileName)
	commonResolved, commonOriginal := cache.guessDirectorySymlink(resolvedFileName, originalPath, cache.cwd)
	if commonResolved != "" && commonOriginal != "" {
		symlinkPath := tspath.ToPath(commonOriginal, cache.cwd, cache.useCaseSensitiveFileNames)
		if !tspath.ContainsIgnoredPath(string(symlinkPath)) {
			cache.SetDirectory(
				commonOriginal,
				symlinkPath.EnsureTrailingDirectorySeparator(),
				&KnownDirectoryLink{
					Real:     tspath.EnsureTrailingDirectorySeparator(commonResolved),
					RealPath: tspath.ToPath(commonResolved, cache.cwd, cache.useCaseSensitiveFileNames).EnsureTrailingDirectorySeparator(),
				},
			)
		}
	}
}

func (cache *KnownSymlinks) guessDirectorySymlink(a string, b string, cwd string) (string, string) {
	aParts := tspath.GetPathComponents(tspath.GetNormalizedAbsolutePath(a, cwd), "")
	bParts := tspath.GetPathComponents(tspath.GetNormalizedAbsolutePath(b, cwd), "")
	isDirectory := false
	for len(aParts) >= 2 && len(bParts) >= 2 &&
		!cache.isNodeModulesOrScopedPackageDirectory(aParts[len(aParts)-2]) &&
		!cache.isNodeModulesOrScopedPackageDirectory(bParts[len(bParts)-2]) &&
		tspath.GetCanonicalFileName(aParts[len(aParts)-1], cache.useCaseSensitiveFileNames) == tspath.GetCanonicalFileName(bParts[len(bParts)-1], cache.useCaseSensitiveFileNames) {
		aParts = aParts[:len(aParts)-1]
		bParts = bParts[:len(bParts)-1]
		isDirectory = true
	}
	if isDirectory {
		return tspath.GetPathFromPathComponents(aParts), tspath.GetPathFromPathComponents(bParts)
	}
	return "", ""
}

// KLUDGE: Don't assume one 'node_modules' links to another. More likely a single directory inside the node_modules is the symlink.
// ALso, don't assume that an `@foo` directory is linked. More likely the contents of that are linked.
func (cache *KnownSymlinks) isNodeModulesOrScopedPackageDirectory(s string) bool {
	return s != "" && (tspath.GetCanonicalFileName(s, cache.useCaseSensitiveFileNames) == "node_modules" || strings.HasPrefix(s, "@"))
}

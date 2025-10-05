package symlinks

import (
	"github.com/microsoft/typescript-go/internal/collections"
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
	directories             collections.SyncMap[tspath.Path, *KnownDirectoryLink]
	files                   collections.SyncMap[tspath.Path, string]
	HasProcessedResolutions bool
}

/** Gets a map from symlink to realpath. Keys have trailing directory separators. */
func (cache *KnownSymlinks) Directories() *collections.SyncMap[tspath.Path, *KnownDirectoryLink] {
	return &cache.directories
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

	// !!!
	// if realDirectory != nil {
	// 	if _, ok := cache.directories.Load(symlinkPath); !ok {
	// 		cache.directoriesByRealpath.Add(realDirectory.RealPath, symlink)
	// 	}
	// }
	cache.directories.Store(symlinkPath, realDirectory)
}

func (cache *KnownSymlinks) SetFile(symlinkPath tspath.Path, realpath string) {
	cache.files.Store(symlinkPath, realpath)
}

// getSymlinkCache(): SymlinkCache {
//     if (!this.symlinks) {
//         this.symlinks = createSymlinkCache(this.getCurrentDirectory(), this.getCanonicalFileName);
//     }
//     if (this.program && !this.symlinks.hasProcessedResolutions()) {
//         this.symlinks.setSymlinksFromResolutions(
//             this.program.forEachResolvedModule,
//             this.program.forEachResolvedTypeReferenceDirective,
//             this.program.getAutomaticTypeDirectiveResolutions(),
//         );
//     }
//     return this.symlinks;
// }

func NewKnownSymlink(
	currentDirectory string,
	canonicalFilename func(string) string,
) *KnownSymlinks {
	return &KnownSymlinks{}
}

// forEachResolvedModule: (
//     callback: (resolution: ResolvedModuleWithFailedLookupLocations, moduleName: string, mode: ResolutionMode, filePath: Path) => void,
// ) => void,
// func (cache *KnownSymlinks) SetSymlinksFromResolutions(func()) bool {
// 	cache.HasProcessedResolutions = true
// }

// setSymlinksFromResolutions(forEachResolvedModule, forEachResolvedTypeReferenceDirective, typeReferenceDirectives) {
//     Debug.assert(!hasProcessedResolutions);
//     hasProcessedResolutions = true;
//     forEachResolvedModule(resolution => processResolution(this, resolution.resolvedModule));
//     forEachResolvedTypeReferenceDirective(resolution => processResolution(this, resolution.resolvedTypeReferenceDirective));
//     typeReferenceDirectives.forEach(resolution => processResolution(this, resolution.resolvedTypeReferenceDirective));
// },

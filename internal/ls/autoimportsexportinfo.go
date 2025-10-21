package ls

import (
	"context"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/collections"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/scanner"
	"github.com/microsoft/typescript-go/internal/stringutil"
)

func (l *LanguageService) getExportInfos(
	ctx context.Context,
	ch *checker.Checker,
	importingFile *ast.SourceFile,
	exportMapKey ExportInfoMapKey,
) []*SymbolExportInfo {
	expInfoMap := NewExportInfoMap(l.GetProgram().GetGlobalTypingsCacheLocation())
	moduleCount := 0
	symbolNameMatch := func(symbolName string) bool {
		return symbolName == exportMapKey.SymbolName
	}
	forEachExternalModuleToImportFrom(
		ch,
		l.GetProgram(),
		// /*useAutoImportProvider*/ true,
		func(moduleSymbol *ast.Symbol, moduleFile *ast.SourceFile, ch *checker.Checker, isFromPackageJson bool) {
			if moduleCount = moduleCount + 1; moduleCount%100 == 0 && ctx.Err() != nil {
				return
			}
			if moduleFile == nil && stringutil.StripQuotes(moduleSymbol.Name) != exportMapKey.AmbientModuleName {
				return
			}
			seenExports := collections.Set[string]{}
			defaultInfo := getDefaultLikeExportInfo(moduleSymbol, ch)
			var exportingModuleSymbol *ast.Symbol
			if defaultInfo != nil {
				exportingModuleSymbol = defaultInfo.exportingModuleSymbol
				// Note: I think we shouldn't actually see resolved module symbols here, but weird merges
				// can cause it to happen: see 'completionsImport_mergedReExport.ts'
				if isImportableSymbol(exportingModuleSymbol, ch) {
					expInfoMap.add(
						importingFile.Path(),
						exportingModuleSymbol,
						core.IfElse(defaultInfo.exportKind == ExportKindDefault, ast.InternalSymbolNameDefault, ast.InternalSymbolNameExportEquals),
						moduleSymbol,
						moduleFile,
						defaultInfo.exportKind,
						isFromPackageJson,
						ch,
						symbolNameMatch,
						nil,
					)
				}
			}
			ch.ForEachExportAndPropertyOfModule(moduleSymbol, func(exported *ast.Symbol, key string) {
				if exported != exportingModuleSymbol && isImportableSymbol(exported, ch) && seenExports.AddIfAbsent(key) {
					expInfoMap.add(
						importingFile.Path(),
						exported,
						key,
						moduleSymbol,
						moduleFile,
						ExportKindNamed,
						isFromPackageJson,
						ch,
						symbolNameMatch,
						nil,
					)
				}
			})
		})
	return expInfoMap.get(importingFile.Path(), ch, exportMapKey)
}

func symbolFlagsHaveMeaning(flags ast.SymbolFlags, meaning ast.SemanticMeaning) bool {
	return meaning == ast.SemanticMeaningAll ||
		(meaning&ast.SemanticMeaningValue != 0 && (flags&ast.SymbolFlagsValue) != 0) ||
		(meaning&ast.SemanticMeaningType != 0 && (flags&ast.SymbolFlagsType) != 0) ||
		(meaning&ast.SemanticMeaningNamespace != 0 && (flags&ast.SymbolFlagsNamespace) != 0)
}

func getUniqueSymbolId(symbol *ast.Symbol, ch *checker.Checker) ast.SymbolId {
	return ast.GetSymbolId(ch.SkipAlias(symbol))
}

// Returns a map from an exported symbol's ID to a list of every way it's (re-)exported.
func (l *LanguageService) getExportInfosMap(
	symbolName string,
	isJsxTagName bool,
	currentTokenMeaning ast.SemanticMeaning,
	fromFile *ast.SourceFile,
	ctx context.Context,
	ch *checker.Checker,
	compilerOptions *core.CompilerOptions,
) *collections.MultiMap[ast.SymbolId, *SymbolExportInfo] {
	// For each original symbol, keep all re-exports of that symbol together so we can call `getCodeActionsForImport` on the whole group at once.
	// Maps symbol id to info for modules providing that symbol (original export + re-exports).
	originalSymbolToExportInfos := &collections.MultiMap[ast.SymbolId, *SymbolExportInfo]{}
	packageJsonFilter := l.createPackageJsonImportFilter(fromFile)
	addSymbol := func(moduleSymbol *ast.Symbol, toFile *ast.SourceFile, exportedSymbol *ast.Symbol, exportKind ExportKind, isFromPackageJson bool) {
		if l.isImportable(
			fromFile,
			toFile,
			moduleSymbol,
			packageJsonFilter,
		) {
			moduleFileName := ""
			if toFile != nil {
				moduleFileName = toFile.FileName()
			}
			originalSymbolToExportInfos.Add(getUniqueSymbolId(exportedSymbol, ch), &SymbolExportInfo{
				symbol:            exportedSymbol,
				moduleSymbol:      moduleSymbol,
				moduleFileName:    moduleFileName,
				exportKind:        exportKind,
				targetFlags:       ch.SkipAlias(exportedSymbol).Flags,
				isFromPackageJson: isFromPackageJson,
			})
		}
	}
	forEachExternalModuleToImportFrom(
		ch,
		l.GetProgram(),
		// /*useAutoImportProvider*/ true,
		func(moduleSymbol *ast.Symbol, moduleFile *ast.SourceFile, ch *checker.Checker, isFromPackageJson bool) {
			defaultInfo := getDefaultLikeExportInfo(moduleSymbol, ch)
			if defaultInfo != nil &&
				symbolFlagsHaveMeaning(ch.GetSymbolFlags(defaultInfo.exportingModuleSymbol), currentTokenMeaning) &&
				forEachNameOfDefaultExport(defaultInfo.exportingModuleSymbol, ch, compilerOptions.GetEmitScriptTarget(), func(name, capitalizedName string) string {
					if isJsxTagName {
						if capitalizedName != "" {
							if capitalizedName == symbolName {
								return capitalizedName
							}
						} else if name == symbolName {
							return name
						}
					} else if name == symbolName {
						return name
					}

					return ""
				}) != "" {
				addSymbol(moduleSymbol, moduleFile, defaultInfo.exportingModuleSymbol, defaultInfo.exportKind, isFromPackageJson)
			}

			// check exports with the same name
			exportSymbolWithIdenticalName := ch.TryGetMemberInModuleExportsAndProperties(symbolName, moduleSymbol)
			if exportSymbolWithIdenticalName != nil && symbolFlagsHaveMeaning(ch.GetSymbolFlags(exportSymbolWithIdenticalName), currentTokenMeaning) {
				addSymbol(moduleSymbol, moduleFile, exportSymbolWithIdenticalName, ExportKindNamed, isFromPackageJson)
			}
		})
	return originalSymbolToExportInfos
}

func (l *LanguageService) searchExportInfosForCompletions(
	ctx context.Context,
	ch *checker.Checker,
	importingFile *ast.SourceFile,
	isForImportStatementCompletion bool,
	isRightOfOpenTag bool,
	isTypeOnlyLocation bool,
	lowerCaseTokenText string,
	action func([]*SymbolExportInfo, string, bool, ExportInfoMapKey) []*SymbolExportInfo,
) {
	symbolNameMatches := map[string]bool{}
	symbolNameMatch := func(symbolName string) bool {
		if !scanner.IsIdentifierText(symbolName, importingFile.LanguageVariant) {
			return false
		}
		if b, ok := symbolNameMatches[symbolName]; ok {
			return b
		}
		if isNonContextualKeyword(scanner.StringToToken(symbolName)) {
			symbolNameMatches[symbolName] = false
			return false
		}
		// Do not try to auto-import something with a lowercase first letter for a JSX tag
		firstChar := rune(symbolName[0])
		if isRightOfOpenTag && (firstChar < 'A' || firstChar > 'Z') {
			symbolNameMatches[symbolName] = false
			return false
		}

		symbolNameMatches[symbolName] = charactersFuzzyMatchInString(symbolName, lowerCaseTokenText)
		return symbolNameMatches[symbolName]
	}
	flagMatch := func(targetFlags ast.SymbolFlags) bool {
		if !isTypeOnlyLocation && !isForImportStatementCompletion && (targetFlags&ast.SymbolFlagsValue) == 0 {
			return false
		}
		if isTypeOnlyLocation && (targetFlags&(ast.SymbolFlagsModule|ast.SymbolFlagsType) == 0) {
			return false
		}
		return true
	}

	expInfoMap := NewExportInfoMap(l.GetProgram().GetGlobalTypingsCacheLocation())
	moduleCount := 0
	forEachExternalModuleToImportFrom(
		ch,
		l.GetProgram(),
		// /*useAutoImportProvider*/ true,
		func(moduleSymbol *ast.Symbol, moduleFile *ast.SourceFile, ch *checker.Checker, isFromPackageJson bool) {
			if moduleCount = moduleCount + 1; moduleCount%100 == 0 && ctx.Err() != nil {
				return
			}
			seenExports := collections.Set[string]{}
			defaultInfo := getDefaultLikeExportInfo(moduleSymbol, ch)
			// Note: I think we shouldn't actually see resolved module symbols here, but weird merges
			// can cause it to happen: see 'completionsImport_mergedReExport.ts'
			if defaultInfo != nil && isImportableSymbol(defaultInfo.exportingModuleSymbol, ch) {
				expInfoMap.add(
					importingFile.Path(),
					defaultInfo.exportingModuleSymbol,
					core.IfElse(defaultInfo.exportKind == ExportKindDefault, ast.InternalSymbolNameDefault, ast.InternalSymbolNameExportEquals),
					moduleSymbol,
					moduleFile,
					defaultInfo.exportKind,
					isFromPackageJson,
					ch,
					symbolNameMatch,
					flagMatch,
				)
			}
			var exportingModuleSymbol *ast.Symbol
			if defaultInfo != nil {
				exportingModuleSymbol = defaultInfo.exportingModuleSymbol
			}
			ch.ForEachExportAndPropertyOfModule(moduleSymbol, func(exported *ast.Symbol, key string) {
				if exported != exportingModuleSymbol && isImportableSymbol(exported, ch) && seenExports.AddIfAbsent(key) {
					expInfoMap.add(
						importingFile.Path(),
						exported,
						key,
						moduleSymbol,
						moduleFile,
						ExportKindNamed,
						isFromPackageJson,
						ch,
						symbolNameMatch,
						flagMatch,
					)
				}
			})
		})
	expInfoMap.search(
		ch,
		importingFile.Path(),
		/*preferCapitalized*/ isRightOfOpenTag,
		func(symbolName string, targetFlags ast.SymbolFlags) bool {
			return symbolNameMatch(symbolName) && flagMatch(targetFlags)
		},
		action,
	)
}

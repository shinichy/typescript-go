package ls

import (
	"context"
	"slices"
	"sort"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/astnav"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/compiler"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/debug"
	"github.com/microsoft/typescript-go/internal/diagnostics"
	"github.com/microsoft/typescript-go/internal/scanner"
	"github.com/microsoft/typescript-go/internal/stringutil"
	"github.com/microsoft/typescript-go/internal/tspath"
)

type Import struct {
	name          string
	kind          ImportKind // ImportKindCommonJS | ImportKindNamespace
	addAsTypeOnly AddAsTypeOnly
	propertyName  string // Use when needing to generate an `ImportSpecifier with a `propertyName`; the name preceding "as" keyword (propertyName = "" when "as" is absent)
}

type FixInfo struct {
	fix                 *ImportFix
	symbolName          string
	errorIdentifierText *string
	isJsxNamespaceFix   *bool
}

func (ct *changeTracker) addNamespaceQualifier(sourceFile *ast.SourceFile, qualification *Qualification) {
	ct.insertText(sourceFile, qualification.usagePosition, qualification.namespacePrefix+".")
}

func (ct *changeTracker) doAddExistingFix(
	sourceFile *ast.SourceFile,
	clause *ast.Node, // ImportClause | ObjectBindingPattern,
	defaultImport *Import,
	namedImports []*Import,
	// removeExistingImportSpecifiers *core.Set[ImportSpecifier | BindingElement] // !!! remove imports not implemented
) {
	switch clause.Kind {
	case ast.KindObjectBindingPattern:
		if clause.Kind == ast.KindObjectBindingPattern {
			// bindingPattern := clause.AsBindingPattern()
			// !!! adding *and* removing imports not implemented
			// if (removeExistingImportSpecifiers && core.Some(bindingPattern.Elements, func(e *ast.Node) bool {
			//     return removeExistingImportSpecifiers.Has(e)
			// })) {
			// If we're both adding and removing elements, just replace and reprint the whole
			// node. The change tracker doesn't understand all the operations and can insert or
			// leave behind stray commas.
			// ct.replaceNode(
			//     sourceFile,
			//     bindingPattern,
			// ct.NodeFactory.NewObjectBindingPattern([
			//     ...bindingPattern.Elements.Filter(func(e *ast.Node) bool {
			//         return !removeExistingImportSpecifiers.Has(e)
			//     }),
			//     ...defaultImport ? [ct.NodeFactory.createBindingElement(/*dotDotDotToken*/ nil, /*propertyName*/ "default", defaultImport.name)] : emptyArray,
			//     ...namedImports.map(i => ct.NodeFactory.createBindingElement(/*dotDotDotToken*/ nil, i.propertyName, i.name)),
			// ]),
			// )
			//     return
			// }
			if defaultImport != nil {
				ct.addElementToBindingPattern(sourceFile, clause, defaultImport.name, ptrTo("default"))
			}
			for _, specifier := range namedImports {
				ct.addElementToBindingPattern(sourceFile, clause, specifier.name, &specifier.propertyName)
			}
			return
		}
	case ast.KindImportClause:

		importClause := clause.AsImportClause()

		// promoteFromTypeOnly = true if we need to promote the entire original clause from type only
		promoteFromTypeOnly := importClause.IsTypeOnly() && core.Some(append(namedImports, defaultImport), func(i *Import) bool {
			if i == nil {
				return false
			}
			return i.addAsTypeOnly == AddAsTypeOnlyNotAllowed
		})

		existingSpecifiers := []*ast.Node{} // []*ast.ImportSpecifier
		if importClause.NamedBindings != nil && importClause.NamedBindings.Kind == ast.KindNamedImports {
			existingSpecifiers = importClause.NamedBindings.Elements()
		}

		if defaultImport != nil {
			debug.Assert(clause.Name() == nil, "Cannot add a default import to an import clause that already has one")
			ct.insertNodeAt(sourceFile, core.TextPos(astnav.GetStartOfNode(clause, sourceFile, false)), ct.NodeFactory.NewIdentifier(defaultImport.name), changeNodeOptions{suffix: ", "})
		}

		if len(namedImports) > 0 {
			specifierComparer, isSorted := ct.ls.getNamedImportSpecifierComparerWithDetection(importClause.Parent, sourceFile)
			newSpecifiers := core.Map(namedImports, func(namedImport *Import) *ast.Node {
				var identifier *ast.Node
				if namedImport.propertyName != "" {
					identifier = ct.NodeFactory.NewIdentifier(namedImport.propertyName).AsIdentifier().AsNode()
				}
				return ct.NodeFactory.NewImportSpecifier(
					(!importClause.IsTypeOnly() || promoteFromTypeOnly) && shouldUseTypeOnly(namedImport.addAsTypeOnly, ct.ls.UserPreferences()),
					identifier,
					ct.NodeFactory.NewIdentifier(namedImport.name),
				)
			})
			slices.SortFunc(newSpecifiers, specifierComparer)

			// !!! remove imports not implemented
			// if (removeExistingImportSpecifiers) {
			//     // If we're both adding and removing specifiers, just replace and reprint the whole
			//     // node. The change tracker doesn't understand all the operations and can insert or
			//     // leave behind stray commas.
			//     ct.replaceNode(
			//         sourceFile,
			//         importClause.NamedBindings,
			//         ct.NodeFactory.updateNamedImports(
			//             importClause.NamedBindings.AsNamedImports(),
			//             append(core.Filter(existingSpecifiers, func (s *ast.ImportSpecifier) bool {return !removeExistingImportSpecifiers.Has(s)}), newSpecifiers...), // !!! sort with specifierComparer
			//         ),
			//     );
			//
			if len(existingSpecifiers) > 0 && isSorted != core.TSFalse {
				// 	The sorting preference computed earlier may or may not have validated that these particular
				// 	import specifiers are sorted. If they aren't, `getImportSpecifierInsertionIndex` will return
				// 	nonsense. So if there are existing specifiers, even if we know the sorting preference, we
				// 	need to ensure that the existing specifiers are sorted according to the preference in order
				// 	to do a sorted insertion.
				//     if we're promoting the clause from type-only, we need to transform the existing imports before attempting to insert the new named imports
				//     transformedExistingSpecifiers := existingSpecifiers
				// 	if promoteFromTypeOnly && existingSpecifiers {
				// 		transformedExistingSpecifiers = ct.NodeFactory.updateNamedImports(
				// 			importClause.NamedBindings.AsNamedImports(),
				// 			core.SameMap(existingSpecifiers, func(e *ast.ImportSpecifier) *ast.ImportSpecifier {
				// 				return ct.NodeFactory.updateImportSpecifier(e, /*isTypeOnly*/ true, e.propertyName, e.name)
				// 			}),
				// 		).elements
				// 	}
				for _, spec := range newSpecifiers {
					insertionIndex := getImportSpecifierInsertionIndex(existingSpecifiers, spec, specifierComparer)
					ct.insertImportSpecifierAtIndex(sourceFile, spec, importClause.NamedBindings, insertionIndex)
				}
			} else if len(existingSpecifiers) > 0 && isSorted.IsTrue() {
				// Existing specifiers are sorted, so insert each new specifier at the correct position
				for _, spec := range newSpecifiers {
					insertionIndex := getImportSpecifierInsertionIndex(existingSpecifiers, spec, specifierComparer)
					if insertionIndex >= len(existingSpecifiers) {
						// Insert at the end
						ct.insertNodeInListAfter(sourceFile, existingSpecifiers[len(existingSpecifiers)-1], spec.AsNode(), existingSpecifiers)
					} else {
						// Insert before the element at insertionIndex
						ct.insertNodeInListAfter(sourceFile, existingSpecifiers[insertionIndex], spec.AsNode(), existingSpecifiers)
					}
				}
			} else if len(existingSpecifiers) > 0 {
				// Existing specifiers may not be sorted, append to the end
				for _, spec := range newSpecifiers {
					ct.insertNodeInListAfter(sourceFile, existingSpecifiers[len(existingSpecifiers)-1], spec.AsNode(), existingSpecifiers)
				}
			} else {
				if len(newSpecifiers) > 0 {
					namedImports := ct.NodeFactory.NewNamedImports(ct.NodeFactory.NewNodeList(newSpecifiers))
					if importClause.NamedBindings != nil {
						ct.replaceNode(sourceFile, importClause.NamedBindings, namedImports, nil)
					} else {
						if clause.Name() == nil {
							panic("Import clause must have either named imports or a default import")
						}
						ct.insertNodeAfter(sourceFile, clause.Name(), namedImports)
					}
				}
			}
		}

		if promoteFromTypeOnly {
			// !!! promote type-only imports not implemented

			// ct.delete(sourceFile, getTypeKeywordOfTypeOnlyImport(clause, sourceFile));
			// if (existingSpecifiers) {
			//     // We used to convert existing specifiers to type-only only if compiler options indicated that
			//     // would be meaningful (see the `importNameElisionDisabled` utility function), but user
			//     // feedback indicated a preference for preserving the type-onlyness of existing specifiers
			//     // regardless of whether it would make a difference in emit.
			//     for _, specifier := range existingSpecifiers {
			//         ct.insertModifierBefore(sourceFile, SyntaxKind.TypeKeyword, specifier);
			//     }
			// }
		}
	default:
		panic("Unsupported clause kind: " + clause.Kind.String() + "for doAddExistingFix")
	}
}

func (ct *changeTracker) addElementToBindingPattern(sourceFile *ast.SourceFile, bindingPattern *ast.Node, name string, propertyName *string) {
	element := ct.newBindingElementFromNameAndPropertyName(name, propertyName)
	if len(bindingPattern.Elements()) > 0 {
		ct.insertNodeInListAfter(sourceFile, bindingPattern.Elements()[len(bindingPattern.Elements())-1], element, nil)
	} else {
		ct.replaceNode(sourceFile, bindingPattern, ct.NodeFactory.NewBindingPattern(
			ast.KindObjectBindingPattern,
			ct.NodeFactory.NewNodeList([]*ast.Node{element}),
		), nil)
	}
}

func (ct *changeTracker) newBindingElementFromNameAndPropertyName(name string, propertyName *string) *ast.Node {
	var newPropertyNameIdentifier *ast.Node
	if propertyName != nil {
		newPropertyNameIdentifier = ct.NodeFactory.NewIdentifier(*propertyName)
	}
	return ct.NodeFactory.NewBindingElement(
		nil, /*dotDotDotToken*/
		newPropertyNameIdentifier,
		ct.NodeFactory.NewIdentifier(name),
		nil, /* initializer */
	)
}

func (ct *changeTracker) insertImports(sourceFile *ast.SourceFile, imports []*ast.Statement, blankLineBetween bool) {
	var existingImportStatements []*ast.Statement

	if imports[0].Kind == ast.KindVariableStatement {
		existingImportStatements = core.Filter(sourceFile.Statements.Nodes, ast.IsRequireVariableStatement)
	} else {
		existingImportStatements = core.Filter(sourceFile.Statements.Nodes, ast.IsAnyImportSyntax)
	}
	comparer, isSorted := ct.ls.getOrganizeImportsStringComparerWithDetection(existingImportStatements)
	sortedNewImports := slices.Clone(imports)
	slices.SortFunc(sortedNewImports, func(a, b *ast.Statement) int {
		return compareImportsOrRequireStatements(a, b, comparer)
	})
	// !!! FutureSourceFile
	// if !isFullSourceFile(sourceFile) {
	//     for _, newImport := range sortedNewImports {
	//         // Insert one at a time to send correct original source file for accurate text reuse
	//         // when some imports are cloned from existing ones in other files.
	//         ct.insertStatementsInNewFile(sourceFile.fileName, []*ast.Node{newImport}, ast.GetSourceFileOfNode(getOriginalNode(newImport)))
	//     }
	// return;
	// }

	if len(existingImportStatements) > 0 && isSorted {
		// Existing imports are sorted, insert each new import at the correct position
		for _, newImport := range sortedNewImports {
			insertionIndex := getImportDeclarationInsertIndex(existingImportStatements, newImport, func(a, b *ast.Statement) stringutil.Comparison {
				return compareImportsOrRequireStatements(a, b, comparer)
			})
			if insertionIndex == 0 {
				// If the first import is top-of-file, insert after the leading comment which is likely the header
				ct.insertNodeAt(sourceFile, core.TextPos(astnav.GetStartOfNode(existingImportStatements[0], sourceFile, false)), newImport.AsNode(), changeNodeOptions{})
			} else {
				prevImport := existingImportStatements[insertionIndex-1]
				ct.insertNodeAfter(sourceFile, prevImport.AsNode(), newImport.AsNode())
			}
		}
	} else if len(existingImportStatements) > 0 {
		ct.insertNodesAfter(sourceFile, existingImportStatements[len(existingImportStatements)-1], sortedNewImports)
	} else {
		ct.insertAtTopOfFile(sourceFile, sortedNewImports, blankLineBetween)
	}
}

func (ct *changeTracker) makeImport(defaultImport *ast.IdentifierNode, namedImports []*ast.Node, moduleSpecifier *ast.Expression, isTypeOnly bool) *ast.Statement {
	var newNamedImports *ast.Node
	if len(namedImports) > 0 {
		newNamedImports = ct.NodeFactory.NewNamedImports(ct.NodeFactory.NewNodeList(namedImports))
	}
	var importClause *ast.Node
	if defaultImport != nil || newNamedImports != nil {
		importClause = ct.NodeFactory.NewImportClause(core.IfElse(isTypeOnly, ast.KindTypeKeyword, ast.KindUnknown), defaultImport, newNamedImports)
	}
	return ct.NodeFactory.NewImportDeclaration( /*modifiers*/ nil, importClause, moduleSpecifier, nil /*attributes*/)
}

func (ct *changeTracker) getNewImports(
	moduleSpecifier string,
	quotePreference quotePreference,
	defaultImport *Import,
	namedImports []*Import,
	namespaceLikeImport *Import, // { importKind: ImportKind.CommonJS | ImportKind.Namespace; }
	compilerOptions *core.CompilerOptions,
) []*ast.Statement {
	moduleSpecifierStringLiteral := ct.NodeFactory.NewStringLiteral(moduleSpecifier)
	if quotePreference == quotePreferenceSingle {
		moduleSpecifierStringLiteral.AsStringLiteral().TokenFlags |= ast.TokenFlagsSingleQuote
	}
	var statements []*ast.Statement // []AnyImportSyntax
	if defaultImport != nil || len(namedImports) > 0 {
		// `verbatimModuleSyntax` should prefer top-level `import type` -
		// even though it's not an error, it would add unnecessary runtime emit.
		topLevelTypeOnly := (defaultImport == nil || needsTypeOnly(defaultImport.addAsTypeOnly)) &&
			core.Every(namedImports, func(i *Import) bool { return needsTypeOnly(i.addAsTypeOnly) }) ||
			(compilerOptions.VerbatimModuleSyntax.IsTrue() || ct.ls.UserPreferences().PreferTypeOnlyAutoImports) &&
				defaultImport != nil && defaultImport.addAsTypeOnly != AddAsTypeOnlyNotAllowed && !core.Some(namedImports, func(i *Import) bool { return i.addAsTypeOnly == AddAsTypeOnlyNotAllowed })

		var defaultImportNode *ast.Node
		if defaultImport != nil {
			defaultImportNode = ct.NodeFactory.NewIdentifier(defaultImport.name)
		}

		statements = append(statements, ct.makeImport(defaultImportNode, core.Map(namedImports, func(namedImport *Import) *ast.Node {
			var namedImportPropertyName *ast.Node
			if namedImport.propertyName != "" {
				namedImportPropertyName = ct.NodeFactory.NewIdentifier(namedImport.propertyName)
			}
			return ct.NodeFactory.NewImportSpecifier(
				!topLevelTypeOnly && shouldUseTypeOnly(namedImport.addAsTypeOnly, ct.ls.UserPreferences()),
				namedImportPropertyName,
				ct.NodeFactory.NewIdentifier(namedImport.name),
			)
		}), moduleSpecifierStringLiteral, topLevelTypeOnly))
	}

	if namespaceLikeImport != nil {
		var declaration *ast.Statement
		if namespaceLikeImport.kind == ImportKindCommonJS {
			declaration = ct.NodeFactory.NewImportEqualsDeclaration(
				/*modifiers*/ nil,
				shouldUseTypeOnly(namespaceLikeImport.addAsTypeOnly, ct.ls.UserPreferences()),
				ct.NodeFactory.NewIdentifier(namespaceLikeImport.name),
				ct.NodeFactory.NewExternalModuleReference(moduleSpecifierStringLiteral),
			)
		} else {
			declaration = ct.NodeFactory.NewImportDeclaration(
				/*modifiers*/ nil,
				ct.NodeFactory.NewImportClause(
					/*phaseModifier*/ core.IfElse(shouldUseTypeOnly(namespaceLikeImport.addAsTypeOnly, ct.ls.UserPreferences()), ast.KindTypeKeyword, ast.KindUnknown),
					/*name*/ nil,
					ct.NodeFactory.NewNamespaceImport(ct.NodeFactory.NewIdentifier(namespaceLikeImport.name)),
				),
				moduleSpecifierStringLiteral,
				/*attributes*/ nil,
			)
		}
		statements = append(statements, declaration)
	}
	if len(statements) == 0 {
		panic("No statements to insert for new imports")
	}
	return statements
}

func needsTypeOnly(addAsTypeOnly AddAsTypeOnly) bool {
	return addAsTypeOnly == AddAsTypeOnlyRequired
}

func shouldUseTypeOnly(addAsTypeOnly AddAsTypeOnly, preferences *UserPreferences) bool {
	return needsTypeOnly(addAsTypeOnly) || addAsTypeOnly != AddAsTypeOnlyNotAllowed && preferences.PreferTypeOnlyAutoImports
}

func (l *LanguageService) getFixInfos(
	ctx context.Context,
	errorCode int32,
	sourceFile *ast.SourceFile,
	position int,
) []*FixInfo {
	ch, done := l.GetProgram().GetTypeCheckerForFile(ctx, sourceFile)
	defer done()

	symbolToken := astnav.GetTokenAtPosition(sourceFile, position)
	var info []*FixInfo

	if errorCode == diagnostics.X_0_refers_to_a_UMD_global_but_the_current_file_is_a_module_Consider_adding_an_import_instead.Code() {
		info = l.getFixesInfoForUMDImport(ch, sourceFile, symbolToken)
	} else if !ast.IsIdentifier(symbolToken) {
		return []*FixInfo{}
	} else if errorCode == diagnostics.X_0_cannot_be_used_as_a_value_because_it_was_imported_using_import_type.Code() {
		symbolName := getSymbolNamesToImport(sourceFile, ch, symbolToken, l.GetProgram().Options())[0]
		fix := getTypeOnlyPromotionFix(ch, sourceFile, symbolToken, symbolName)
		return []*FixInfo{{fix: fix, symbolName: symbolName, errorIdentifierText: ptrTo(symbolToken.Text())}}
	} else {
		info = l.getFixesInfoForNonUMDImport(ctx, ch, sourceFile, symbolToken.AsIdentifier())
	}

	packageJsonImportFilter := l.createPackageJsonImportFilter(sourceFile)
	return l.sortFixInfo(info, sourceFile, l.program, packageJsonImportFilter)
}

func (l *LanguageService) sortFixInfo(fixes []*FixInfo, sourceFile *ast.SourceFile, program *compiler.Program, packageJsonImportFilter *packageJsonImportFilter) []*FixInfo {
	_toPath := func(fileName string) tspath.Path {
		return tspath.ToPath(fileName, program.GetCurrentDirectory(), program.UseCaseSensitiveFileNames())
	}
	sort.Slice(fixes, func(i, j int) bool {
		return compareBooleans(
			fixes[i].isJsxNamespaceFix != nil && *fixes[i].isJsxNamespaceFix,
			fixes[j].isJsxNamespaceFix != nil && *fixes[j].isJsxNamespaceFix) < 0 ||
			compareValues(int(fixes[i].fix.kind), int(fixes[j].fix.kind)) < 0 ||
			l.compareModuleSpecifiers(fixes[i].fix, fixes[j].fix, sourceFile, packageJsonImportFilter.allowsImportingSpecifier, _toPath) < 0
	})
	return fixes
}

func (l *LanguageService) getFixesInfoForUMDImport(
	ch *checker.Checker,
	sourceFile *ast.SourceFile,
	token *ast.Node,
) []*FixInfo {
	umdSymbol := getUmdSymbol(token, ch)
	if umdSymbol == nil {
		return []*FixInfo{}
	}
	symbol := ch.GetAliasedSymbol(umdSymbol)
	symbolName := umdSymbol.Name
	exportInfo := []*SymbolExportInfo{{
		symbol:            umdSymbol,
		moduleSymbol:      symbol,
		moduleFileName:    "",
		exportKind:        ExportKindUMD,
		targetFlags:       symbol.Flags,
		isFromPackageJson: false,
	}}
	useRequire := getShouldUseRequire(sourceFile, l.GetProgram())
	// `usagePosition` is undefined because `token` may not actually be a usage of the symbol we're importing.
	// For example, we might need to import `React` in order to use an arbitrary JSX tag. We could send a position
	// for other UMD imports, but `usagePosition` is currently only used to insert a namespace qualification
	// before a named import, like converting `writeFile` to `fs.writeFile` (whether `fs` is already imported or
	// not), and this function will only be called for UMD symbols, which are necessarily an `export =`, not a
	// named export.
	_, fixes := l.getImportFixes(
		ch,
		exportInfo,
		nil,          /* usagePosition */
		ptrTo(false), /* isValidTypeOnlyUseSite */
		ptrTo(useRequire),
		sourceFile,
		false, /* fromCacheOnly */
	)
	result := []*FixInfo{}
	for _, fix := range fixes {
		var errorIdentifierText *string
		if ast.IsIdentifier(token) {
			errorIdentifierText = ptrTo(token.AsIdentifier().Text)
		}
		result = append(result, &FixInfo{
			fix:                 fix,
			symbolName:          symbolName,
			errorIdentifierText: errorIdentifierText,
		})
	}
	return result
}

func getUmdSymbol(token *ast.Node, ch *checker.Checker) *ast.Symbol {
	// try the identifier to see if it is the umd symbol
	var umdSymbol *ast.Symbol
	if ast.IsIdentifier(token) {
		umdSymbol = ch.GetSymbolAtLocation(token)
	}
	if checker.IsUMDExportSymbol(umdSymbol) {
		return umdSymbol
	}

	// The error wasn't for the symbolAtLocation, it was for the JSX tag itself, which needs access to e.g. `React`.
	parent := token.Parent
	if (ast.IsJsxOpeningLikeElement(parent) && parent.TagName() == token) || ast.IsJsxOpeningFragment(parent) {
		var location *ast.Node
		if ast.IsJsxOpeningFragment(parent) {
			location = token
		} else {
			location = parent
		}
		parentSymbol := ch.ResolveName(
			ch.GetJsxNamespace(parent),
			location,
			ast.SymbolFlagsValue,
			/*excludeGlobals*/ false,
		)
		if checker.IsUMDExportSymbol(parentSymbol) {
			return parentSymbol
		}
	}
	return nil
}

func (l *LanguageService) getFixesInfoForNonUMDImport(
	ctx context.Context,
	ch *checker.Checker,
	sourceFile *ast.SourceFile,
	symbolToken *ast.Identifier,
) []*FixInfo {
	symbolName := symbolToken.Text
	// "default" is a keyword and not a legal identifier for the import, but appears as an identifier.
	if symbolName == ast.InternalSymbolNameDefault {
		return []*FixInfo{}
	}
	isValidTypeOnlyUseSite := ast.IsValidTypeOnlyAliasUseSite(symbolToken.AsNode())
	useRequire := getShouldUseRequire(sourceFile, l.GetProgram())
	exportInfosMap := l.getExportInfosMap(symbolName, ast.IsJsxTagName(symbolToken.AsNode()), getMeaningFromLocation(symbolToken.AsNode()), sourceFile, ctx, ch, l.GetProgram().Options())
	result := []*FixInfo{}
	for values := range exportInfosMap.Values() {
		_, fixes := l.getImportFixes(ch,
			values,
			ptrTo(l.converters.PositionToLineAndCharacter(sourceFile, core.TextPos(symbolToken.Loc.Pos()))),
			ptrTo(isValidTypeOnlyUseSite),
			ptrTo(useRequire),
			sourceFile,
			false /* fromCacheOnly */)
		for _, fix := range fixes {
			result = append(result, &FixInfo{
				fix:                 fix,
				symbolName:          symbolName,
				errorIdentifierText: ptrTo(symbolToken.Text),
				isJsxNamespaceFix:   ptrTo(symbolName != symbolToken.Text),
			})
		}
	}
	return result
}

func getTypeOnlyPromotionFix(ch *checker.Checker, sourceFile *ast.SourceFile, symbolToken *ast.IdentifierNode, symbolName string) *ImportFix /*FixPromoteTypeOnlyImport*/ {
	symbol := ch.ResolveName(
		symbolName,
		symbolToken,
		ast.SymbolFlagsValue,
		/*excludeGlobals*/ true,
	)
	if symbol == nil {
		return nil
	}

	typeOnlyAliasDeclaration := ch.GetTypeOnlyAliasDeclaration(symbol)
	if typeOnlyAliasDeclaration == nil || ast.GetSourceFileOfNode(typeOnlyAliasDeclaration) != sourceFile {
		return nil
	}

	return getNewPromoteTypeOnlyImport(typeOnlyAliasDeclaration)
}

func getSymbolNamesToImport(sourceFile *ast.SourceFile, ch *checker.Checker, symbolToken *ast.IdentifierNode, compilerOptions *core.CompilerOptions) []string {
	parent := symbolToken.Parent
	if (ast.IsJsxOpeningLikeElement(parent) || ast.IsJsxClosingElement(parent)) && parent.TagName() == symbolToken && jsxModeNeedsExplicitImport(compilerOptions.Jsx) {
		jsxNamespace := ch.GetJsxNamespace(sourceFile.AsNode())
		if needsJsxNamespaceFix(jsxNamespace, symbolToken, ch) {
			needsComponentNameFix := !scanner.IsIntrinsicJsxName(symbolToken.Text()) && ch.ResolveName(
				symbolToken.Text(),
				symbolToken,
				ast.SymbolFlagsValue,
				/*excludeGlobals*/ false,
			) == nil
			if needsComponentNameFix {
				return []string{symbolToken.Text(), jsxNamespace}
			}
			return []string{jsxNamespace}
		}
	}
	return []string{symbolToken.Text()}
}

func jsxModeNeedsExplicitImport(jsx core.JsxEmit) bool {
	return jsx == core.JsxEmitReact || jsx == core.JsxEmitReactNative
}

func needsJsxNamespaceFix(jsxNamespace string, symbolToken *ast.IdentifierNode, ch *checker.Checker) bool {
	if scanner.IsIntrinsicJsxName(symbolToken.Text()) {
		return true // If we were triggered by a matching error code on an intrinsic, the error must have been about missing the JSX factory
	}
	namespaceSymbol := ch.ResolveName(
		jsxNamespace,
		symbolToken,
		ast.SymbolFlagsValue,
		/*excludeGlobals*/ true,
	)
	return namespaceSymbol == nil || core.Some(namespaceSymbol.Declarations, func(decl *ast.Node) bool {
		return ast.IsTypeOnlyImportOrExportDeclaration(decl)
	}) && (namespaceSymbol.Flags&ast.SymbolFlagsValue) == 0
}

// Compare two numeric values for their order relative to each other.
func compareValues(a, b int) int {
	if a == b {
		return 0
	}
	if a < b {
		return -1
	}
	return 1
}

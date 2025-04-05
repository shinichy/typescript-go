package ls

import (
	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/astnav"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/scanner"
)

func (l *LanguageService) ProvideDefinitions(fileName string, position int) []Location {
	program, file := l.getProgramAndFile(fileName)
	node := astnav.GetTouchingPropertyName(file, position)
	if node.Kind == ast.KindSourceFile {
		return nil
	}

	checker := program.GetTypeChecker()
	calledDeclaration := l.TryGetSignatureDeclaration(checker, node)
	if calledDeclaration != nil {
		name := ast.GetNameOfDeclaration(calledDeclaration)
		if name != nil {
			return createLocationsFromDeclarations([]*ast.Node{name})
		}
	}

	if symbol := checker.GetSymbolAtLocation(node); symbol != nil {
		if symbol.Flags&ast.SymbolFlagsAlias != 0 {
			if resolved, ok := checker.ResolveAlias(symbol); ok {
				symbol = resolved
			}
		}

		return createLocationsFromDeclarations(symbol.Declarations)
	}
	return nil
}

func createLocationsFromDeclarations(declarations []*ast.Node) []Location {
	locations := make([]Location, 0, len(declarations))
	for _, decl := range declarations {
		file := ast.GetSourceFileOfNode(decl)
		loc := decl.Loc
		pos := scanner.GetTokenPosOfNode(decl, file, false /*includeJSDoc*/)

		locations = append(locations, Location{
			FileName: file.FileName(),
			Range:    core.NewTextRange(pos, loc.End()),
		})
	}
	return locations
}

func (l *LanguageService) isRightSideOfPropertyAccess(node *ast.Node) bool {
	parent := node.Parent
	return ast.IsPropertyAccessExpression(parent) && parent.AsPropertyAccessExpression().Name() == node
}

/** Returns a CallLikeExpression where `node` is the target being invoked. */
func (l *LanguageService) getAncestorCallLikeExpression(node *ast.Node) *ast.Node {
	target := ast.FindAncestor(node, func(n *ast.Node) bool {
		return !l.isRightSideOfPropertyAccess(n)
	})

	callLike := target.Parent
	if callLike != nil && checker.IsCallLikeExpression(callLike) && checker.GetInvokedExpression(callLike) == target {
		return callLike
	}

	return nil
}

func (l *LanguageService) TryGetSignatureDeclaration(typeChecker *checker.Checker, node *ast.Node) *ast.Node {
	callLike := l.getAncestorCallLikeExpression(node)
	var signature *checker.Signature
	if callLike != nil {
		signature = typeChecker.GetResolvedSignature(callLike, nil, checker.CheckModeNormal)
	}

	// Don't go to a function type, go to the value having that type.
	var declaration *ast.Node
	if signature != nil && checker.GetDeclaration(signature) != nil {
		declaration = checker.GetDeclaration(signature)
		if ast.IsFunctionLike(declaration) && !ast.IsFunctionTypeNode(declaration) {
			return declaration
		}
	}

	return nil
}

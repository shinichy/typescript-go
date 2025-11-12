package ls

import (
	"context"

	"github.com/microsoft/typescript-go/internal/collections"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/debug"
	"github.com/microsoft/typescript-go/internal/lsp/lsproto"
)

type CodeFixRegistration struct {
	errorCodes     []int32
	getCodeActions func(l *LanguageService, ctx context.Context, documentURI lsproto.DocumentUri, documentPosition lsproto.Range, errorCode int32) *[]lsproto.CommandOrCodeAction
	fixIds         []string
	// getAllCodeActions func(context CodeFixAllContext) *CombinedCodeActions
}

type CodeFixProvider struct {
	errorCodeToFixes    *collections.MultiMap[string, *CodeFixRegistration]
	fixIdToRegistration map[string]*CodeFixRegistration
}

func NewCodeFixProvider() *CodeFixProvider {
	provider := &CodeFixProvider{
		errorCodeToFixes:    &collections.MultiMap[string, *CodeFixRegistration]{},
		fixIdToRegistration: map[string]*CodeFixRegistration{},
	}
	RegisterAutoImportCodeFix(provider)
	return provider
}

func (c *CodeFixProvider) RegisterCodeFix(reg *CodeFixRegistration) {
	for _, errorCode := range reg.errorCodes {
		c.errorCodeToFixes.Add(string(errorCode), reg)
	}
	for _, fixId := range reg.fixIds {
		debug.Assert(c.fixIdToRegistration != nil)
		debug.Assert(c.fixIdToRegistration[fixId] == nil)
		c.fixIdToRegistration[fixId] = reg
	}
}

func (l *LanguageService) ProvideCodeActions(ctx context.Context, params *lsproto.CodeActionParams) (lsproto.CodeActionResponse, error) {
	diagnostics := params.Context.Diagnostics
	if len(diagnostics) == 0 {
		return lsproto.CommandOrCodeActionArrayOrNull{}, nil
	}
	// todo: handle multiple diagnostics
	errorCode := diagnostics[0].Code.Integer
	if errorCode == nil {
		return lsproto.CommandOrCodeActionArrayOrNull{}, nil
	}
	registrations := l.codeFixProvider.errorCodeToFixes.Get(string(*errorCode))
	codeActionArray := core.FlatMap(registrations, func(reg *CodeFixRegistration) []lsproto.CommandOrCodeAction {
		return *reg.getCodeActions(l, ctx, params.TextDocumentURI(), params.Range, *errorCode)
	})

	return lsproto.CommandOrCodeActionArrayOrNull{
		CommandOrCodeActionArray: &codeActionArray,
	}, nil
}

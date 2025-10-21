package ls

import (
	"context"

	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/lsp/lsproto"
)

func (l *LanguageService) ProvideCodeActions(ctx context.Context, documentURI lsproto.DocumentUri, documentPosition lsproto.Range, errorCode int32) (lsproto.CodeActionResponse, error) {
	_, file := l.getProgramAndFile(documentURI)
	position := int(l.converters.LineAndCharacterToPosition(file, documentPosition.Start))
	info := l.getFixInfos(ctx, errorCode, file, position)
	codeActionArray := core.Map(info, func(fix *FixInfo) lsproto.CommandOrCodeAction {
		action := l.codeActionForFix(
			ctx,
			file,
			fix.symbolName,
			fix.fix,
			/*includeSymbolNameInDescription*/ fix.errorIdentifierText == nil || fix.symbolName != *fix.errorIdentifierText)
		kind := lsproto.CodeActionKindQuickFix
		codeAction := &lsproto.CodeAction{
			Title: action.description,
			Kind:  &kind,
			Edit: &lsproto.WorkspaceEdit{
				Changes: &map[lsproto.DocumentUri][]*lsproto.TextEdit{
					documentURI: action.changes,
				},
			},
		}
		return lsproto.CommandOrCodeAction{
			Command:    nil,
			CodeAction: codeAction,
		}
	})

	return lsproto.CommandOrCodeActionArrayOrNull{
		CommandOrCodeActionArray: &codeActionArray,
	}, nil
}

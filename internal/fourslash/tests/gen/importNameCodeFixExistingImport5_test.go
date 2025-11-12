package fourslash_test

import (
	"testing"

	"github.com/microsoft/typescript-go/internal/fourslash"
	"github.com/microsoft/typescript-go/internal/testutil"
)

func TestImportNameCodeFixExistingImport5(t *testing.T) {
	t.Parallel()
	t.Skip()
	defer testutil.RecoverAndFail(t, "Panic on fourslash test")
	const content = `[|import "./module";
f1/*0*/();|]
// @Filename: module.ts
export function f1() {}
export var v1 = 5;`
	f := fourslash.NewFourslash(t, nil /*capabilities*/, content)
	f.VerifyImportFixAtPosition(t, []string{"import \"./module\";\nimport { f1 } from \"./module\";\nf1();"}, nil /*errorCode*/, nil /*preferences*/)
}

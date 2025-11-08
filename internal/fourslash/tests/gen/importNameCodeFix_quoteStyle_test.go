package fourslash_test

import (
	"testing"

	"github.com/microsoft/typescript-go/internal/fourslash"
	"github.com/microsoft/typescript-go/internal/ls"
	"github.com/microsoft/typescript-go/internal/testutil"
)

func TestImportNameCodeFix_quoteStyle(t *testing.T) {
	t.Parallel()

	defer testutil.RecoverAndFail(t, "Panic on fourslash test")
	const content = `// @Filename: /a.ts
export const foo: number;
// @Filename: /b.ts
[|foo;|]`
	f := fourslash.NewFourslash(t, nil /*capabilities*/, content)
	f.GoToFile(t, "/b.ts")
	f.VerifyImportFixAtPosition(t, []string{"import { foo } from './a';\n\nfoo;"}, nil /*errorCode*/, &ls.UserPreferences{QuotePreference: ls.QuotePreference("single")})
}

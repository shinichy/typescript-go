package fourslash_test

import (
	"testing"

	"github.com/microsoft/typescript-go/internal/fourslash"
	"github.com/microsoft/typescript-go/internal/testutil"
)

func TestImportNameCodeFix_tripleSlashOrdering(t *testing.T) {
	t.Parallel()

	defer testutil.RecoverAndFail(t, "Panic on fourslash test")
	const content = `// @Filename: /tsconfig.json
{
    "compilerOptions": {
        "skipDefaultLibCheck": false
    }
}
// @Filename: /a.ts
export const x = 0;
// @Filename: /b.ts
// some comment

/// <reference lib="es2017.string" />

const y = x + 1;
// @Filename: /c.ts
// some comment

/// <reference path="jquery-1.8.3.js" />

const y = x + 1;
// @Filename: /d.ts
// some comment

/// <reference types="node" />

const y = x + 1;
// @Filename: /e.ts
// some comment

/// <reference no-default-lib="true" />

const y = x + 1;
// @Filename: /f.ts
// some comment

/// <amd-module name="NamedModule" />

const y = x + 1;
// @Filename: /g.ts
// some comment

/// <amd-dependency path="legacy/moduleA" name="moduleA" />

const y = x + 1;`
	f := fourslash.NewFourslash(t, nil /*capabilities*/, content)
	f.GoToFile(t, "/b.ts")
	f.VerifyImportFixAtPosition(t, []string{"// some comment\n\n/// <reference lib=\"es2017.string\" />\n\nimport { x } from \"./a\";\n\nconst y = x + 1;"}, nil /*errorCode*/, nil /*preferences*/)
	f.GoToFile(t, "/c.ts")
	f.VerifyImportFixAtPosition(t, []string{"// some comment\n\n/// <reference path=\"jquery-1.8.3.js\" />\n\nimport { x } from \"./a\";\n\nconst y = x + 1;"}, nil /*errorCode*/, nil /*preferences*/)
	f.GoToFile(t, "/d.ts")
	f.VerifyImportFixAtPosition(t, []string{"// some comment\n\n/// <reference types=\"node\" />\n\nimport { x } from \"./a\";\n\nconst y = x + 1;"}, nil /*errorCode*/, nil /*preferences*/)
	f.GoToFile(t, "/e.ts")
	f.VerifyImportFixAtPosition(t, []string{"// some comment\n\n/// <reference no-default-lib=\"true\" />\n\nimport { x } from \"./a\";\n\nconst y = x + 1;"}, nil /*errorCode*/, nil /*preferences*/)
	f.GoToFile(t, "/f.ts")
	f.VerifyImportFixAtPosition(t, []string{"// some comment\n\n/// <amd-module name=\"NamedModule\" />\n\nimport { x } from \"./a\";\n\nconst y = x + 1;"}, nil /*errorCode*/, nil /*preferences*/)
	f.GoToFile(t, "/g.ts")
	f.VerifyImportFixAtPosition(t, []string{"// some comment\n\n/// <amd-dependency path=\"legacy/moduleA\" name=\"moduleA\" />\n\nimport { x } from \"./a\";\n\nconst y = x + 1;"}, nil /*errorCode*/, nil /*preferences*/)
}

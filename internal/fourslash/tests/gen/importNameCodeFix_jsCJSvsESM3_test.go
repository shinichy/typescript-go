package fourslash_test

import (
	"testing"

	"github.com/microsoft/typescript-go/internal/fourslash"
	"github.com/microsoft/typescript-go/internal/testutil"
)

func TestImportNameCodeFix_jsCJSvsESM3(t *testing.T) {
	t.Parallel()

	defer testutil.RecoverAndFail(t, "Panic on fourslash test")
	const content = `// @allowJs: true
// @checkJs: true
// @Filename: types/dep.d.ts
export declare class Dep {}
// @Filename: index.js
import fs from 'fs';
const path = require('path');

Dep/**/
// @Filename: util2.js
export {};`
	f := fourslash.NewFourslash(t, nil /*capabilities*/, content)
	f.GoToMarker(t, "")
	f.VerifyImportFixAtPosition(t, []string{"import fs from 'fs';\nimport { Dep } from './types/dep';\nconst path = require('path');\n\nDep"}, nil /*errorCode*/, nil /*preferences*/)
}

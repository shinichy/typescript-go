package fourslash_test

import (
	"testing"

	"github.com/microsoft/typescript-go/internal/fourslash"
	"github.com/microsoft/typescript-go/internal/testutil"
)

func TestImportNameCodeFix_importType7(t *testing.T) {
	t.Parallel()

	defer testutil.RecoverAndFail(t, "Panic on fourslash test")
	const content = `// @module: es2015
// @Filename: /exports.ts
export interface SomeInterface {}
export class SomePig {}
// @Filename: /a.ts
import {
    type SomeInterface,
    type SomePig,
} from "./exports.js";
new SomePig/**/`
	f := fourslash.NewFourslash(t, nil /*capabilities*/, content)
	f.GoToMarker(t, "")
	f.VerifyImportFixAtPosition(t, []string{"import {\n    SomePig,\n    type SomeInterface,\n} from \"./exports.js\";\nnew SomePig"}, nil /*errorCode*/, nil /*preferences*/)
	f.VerifyImportFixAtPosition(t, []string{"import {\n    SomePig,\n    type SomeInterface,\n} from \"./exports.js\";\nnew SomePig"}, nil /*errorCode*/, nil /*preferences*/)
	f.VerifyImportFixAtPosition(t, []string{"import {\n    type SomeInterface,\n    SomePig,\n} from \"./exports.js\";\nnew SomePig"}, nil /*errorCode*/, nil /*preferences*/)
	f.VerifyImportFixAtPosition(t, []string{"import {\n    type SomeInterface,\n    SomePig,\n} from \"./exports.js\";\nnew SomePig"}, nil /*errorCode*/, nil /*preferences*/)
}

//// [tests/cases/compiler/indexSignatureWithInitializer1.ts] ////

//// [indexSignatureWithInitializer1.ts]
class C {
  [a: number = 1]: number;
}

//// [indexSignatureWithInitializer1.js]
class C {
}

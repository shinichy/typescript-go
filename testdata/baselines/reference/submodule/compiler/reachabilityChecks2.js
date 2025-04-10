//// [tests/cases/compiler/reachabilityChecks2.ts] ////

//// [reachabilityChecks2.ts]
while (true) { }
const enum E { X }

module A4 {
    while (true);
    module A {
        const enum E { X }
    }
}



//// [reachabilityChecks2.js]
while (true) { }
var E;
(function (E) {
    E[E["X"] = 0] = "X";
})(E || (E = {}));
var A4;
(function (A4) {
    while (true)
        ;
})(A4 || (A4 = {}));

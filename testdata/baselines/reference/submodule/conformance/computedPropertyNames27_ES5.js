//// [tests/cases/conformance/es6/computedProperties/computedPropertyNames27_ES5.ts] ////

//// [computedPropertyNames27_ES5.ts]
class Base {
}
class C extends Base {
    [(super(), "prop")]() { }
}

//// [computedPropertyNames27_ES5.js]
class Base {
}
class C extends Base {
    [(super(), "prop")]() { }
}

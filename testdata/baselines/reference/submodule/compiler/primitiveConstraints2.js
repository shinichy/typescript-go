//// [tests/cases/compiler/primitiveConstraints2.ts] ////

//// [primitiveConstraints2.ts]
class C<T> {
   public bar2<U extends T>(x: T, y: U): T {
      return null;
     }
}
 
var x = new C<number>();
x.bar2(2, ""); // should error
x.bar2<string>(2, ""); // should error

//// [primitiveConstraints2.js]
class C {
    bar2(x, y) {
        return null;
    }
}
var x = new C();
x.bar2(2, ""); // should error
x.bar2(2, ""); // should error

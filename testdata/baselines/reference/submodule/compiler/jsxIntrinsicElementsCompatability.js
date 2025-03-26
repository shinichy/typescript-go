//// [tests/cases/compiler/jsxIntrinsicElementsCompatability.tsx] ////

//// [jsxIntrinsicElementsCompatability.tsx]
/// <reference path="/.lib/react16.d.ts" />
import * as React from "react";
function SomeComponent<T extends 'button' | 'a'>(props: { element?: T } & JSX.IntrinsicElements[T]): JSX.Element {
    // Just so the return value is RectElement, the rendered element doesnt matter
    return <div />
}

function Test<T extends 'button' | 'a'>(el: T) {
    return <SomeComponent element={el} />
}

//// [jsxIntrinsicElementsCompatability.js]
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function SomeComponent(props) {
    // Just so the return value is RectElement, the rendered element doesnt matter
    return <div />;
}
function Test(el) {
    return <SomeComponent element={el}/>;
}

//// [tests/cases/compiler/jsxElementClassTooManyParams.tsx] ////

//// [jsxElementClassTooManyParams.tsx]
namespace JSX {
    export interface Element {}
    export interface IntrinsicClassAttributes<TClass, TOther=never> {
        ref?: TClass;
        item?: TOther;
    }
    export interface ElementClass extends Element {}
    export interface ElementAttributesProperty { props: {}; }
    export interface ElementChildrenAttribute { children: {}; }
    export interface IntrinsicAttributes {}
    export interface IntrinsicElements { [key: string]: Element }
}
class ElemClass<T extends {x: number}> implements JSX.ElementClass {
    constructor(public props: T) {}
}
const elem = <ElemClass x={12} y={24} />

//// [jsxElementClassTooManyParams.jsx]
class ElemClass {
    props;
    constructor(props) {
        this.props = props;
    }
}
const elem = <ElemClass x={12} y={24}/>;

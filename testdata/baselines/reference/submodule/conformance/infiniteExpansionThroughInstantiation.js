//// [tests/cases/conformance/types/typeRelationships/recursiveTypes/infiniteExpansionThroughInstantiation.ts] ////

//// [infiniteExpansionThroughInstantiation.ts]
// instantiating a derived type can cause an infinitely expanding type reference to be generated

interface List<T> {
    data: T;
    next: List<T>;
    owner: OwnerList<T>;
}

// will have an owner property that is an infinitely expanding type reference
interface OwnerList<U> extends List<List<U>> {
    name: string;
}

var list: List<string>;
var ownerList: OwnerList<string>;
list = ownerList; 

function other<T>(x: T) {
    var list: List<T>;
    var ownerList: OwnerList<T>;
    list = ownerList; 

}


//// [infiniteExpansionThroughInstantiation.js]
var list;
var ownerList;
list = ownerList;
function other(x) {
    var list;
    var ownerList;
    list = ownerList;
}

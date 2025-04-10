//// [tests/cases/compiler/nestedBlockScopedBindings15.ts] ////

//// [nestedBlockScopedBindings15.ts]
for (; false;) {
    {
        let x;
        () => x;
    }
}

for (; false;) {
    {
        let y;
        y = 1;
    }
}

for (; false;) {
    switch (1){
        case 1:
            let z0;
            () => z0;
            break;
    }
}

for (; false;) {
    switch (1){
        case 1:
            let z;
            z = 1;
            break;
    }
}

//// [nestedBlockScopedBindings15.js]
for (; false;) {
    {
        let x;
        () => x;
    }
}
for (; false;) {
    {
        let y;
        y = 1;
    }
}
for (; false;) {
    switch (1) {
        case 1:
            let z0;
            () => z0;
            break;
    }
}
for (; false;) {
    switch (1) {
        case 1:
            let z;
            z = 1;
            break;
    }
}

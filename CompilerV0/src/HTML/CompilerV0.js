function ifThen(test, trueClause) {
    if (test()) {
        return trueClause();
    }
}
function ifThenElse(test, trueClause, falseClause) {
    if (test()) {
        return trueClause();
    } else {
        return falseClause();
    }
}
function whileLoop(test, body) {
    while (test) {
        body;
    }
}

function println(stmt) {
    document.writeln(stmt);
}
function print(stmt) {
    document.write(stmt);
}

function Nint(num) {
    return num;
}
function Ndouble(num) {
    return num;
}
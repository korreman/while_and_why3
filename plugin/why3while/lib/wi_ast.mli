type coord = {
    line: int;
    col: int;
}

type pos = {
    start: coord;
    stop: coord;
}

type 'a tagged = {
    pos: pos;
    desc: 'a;
}

type var = string

type value =
    | VBool of bool

type binop =
    | Band
    | Bor

type expr =
    | EValue of value tagged
    | EVar of var tagged
    | ENot of expr tagged
    | EBinop of binop tagged * expr tagged * expr tagged

type pred = expr

type invariant = pred

type stmt =
    | SSkip
    | SAssert of pred tagged
    | SAssign of var tagged * expr tagged
    | SIfElse of expr tagged * stmt tagged * stmt tagged
    | SWhile of expr tagged * invariant tagged * stmt tagged

type ast = stmt tagged list

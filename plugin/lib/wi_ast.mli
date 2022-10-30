(* TODO: write or derive a printer *)

type coord = { line : int; col : int }
type pos = { start : coord; stop : coord }
type 'a tagged = { pos : pos; desc : 'a }

type ebinop = BAdd | BSub | BMul | BDiv | BRem
type fbinop = FAnd | FOr | FImplies
type fcmp = CEq | CNe | CGt | CGe | CLt | CLe
type fquant = FForall | FExists

type expr =
  | EConst of int
  | EVar of string
  | EBinop of ebinop tagged * expr tagged * expr tagged

type cond =
  | FTerm of bool
  | FNot of cond tagged
  | FBinop of fbinop tagged * cond tagged * cond tagged
  | FCompare of fcmp tagged * expr tagged * expr tagged
  | FQuant of fquant tagged * string tagged list * cond tagged

type stmt =
  | SSkip
  | SAssert of cond tagged
  | SAssign of string tagged * expr tagged
  | SIfElse of cond tagged * stmt tagged * stmt tagged
  | SWhile of cond tagged * cond tagged * stmt tagged list

type decls = string tagged list
type ast = decls * stmt tagged list

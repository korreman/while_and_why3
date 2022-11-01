type coord = { line : int; col : int }
type pos = { start : coord; stop : coord }
type 'a tagged = { pos : pos; desc : 'a }

type ebinop = BAdd | BSub | BMul | BDiv | BRem
type fbinop = FAnd | FOr | FImplies
type fcmp = CEq | CGt | CGe | CLt | CLe
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
  | SSeq of stmt tagged list
  | SAssert of cond tagged
  | SAssign of string tagged * expr tagged
  | SIfElse of cond tagged * stmt tagged * stmt tagged
  | SWhile of cond tagged * cond tagged * stmt tagged

type decls = string tagged list

type ast = decls * stmt

(*** quick and dirty printing ***)

let show_fop (op: ebinop) =
  match op with
  | BAdd -> " + "
  | BSub -> " - "
  | BMul -> " * "
  | BDiv -> " / "
  | BRem -> " % "

let rec show_expr (e: expr) =
  match e with
  | EConst c -> string_of_int c
  | EVar v -> v
  | EBinop (op, e1, e2) ->
    "(" ^ show_expr e1.desc ^ show_fop op.desc ^ show_expr e2.desc ^ ")"

let show_cop (op: fbinop) =
  match op with
  | FAnd -> " /\\ "
  | FOr -> " \\/ "
  | FImplies -> " -> "

let show_cmp (op: fcmp) =
  match op with
  | CEq -> " = "
  | CGt -> " > "
  | CGe -> " >= "
  | CLt -> " < "
  | CLe -> " <= "

let show_quant (q: fquant) =
  match q with
  | FForall -> "forall "
  | FExists -> "exists "

let rec show_cond (c: cond) =
    match c with
    | FTerm b -> string_of_bool b
    | FNot c -> "not " ^ show_cond c.desc
    | FBinop (op, c1, c2) ->
      "(" ^ show_cond c1.desc ^show_cop op.desc ^show_cond c2.desc ^ ")"
    | FCompare (cmp, e1, e2) ->
        "(" ^ show_expr e1.desc ^show_cmp cmp.desc^ show_expr e2.desc ^ ")"
    | FQuant (q, ss, c) ->
        show_quant q.desc ^
        String.concat " " (List.map (fun v -> v.desc) ss) ^ ". " ^
        show_cond c.desc

let rec show_stmt (s: stmt) =
    (match s with
    | SSkip -> "skip"
    | SSeq ss -> String.concat ";\n" (List.map (fun stmt -> show_stmt stmt.desc) ss) ^ ";\n"
    | SAssert c -> "assert " ^ show_cond c.desc
    | SAssign (v, e) -> v.desc ^ " <- " ^ show_expr e.desc
    | SIfElse (c, s1, s2) ->
        "if " ^ show_cond c.desc ^ " then " ^ show_stmt s1.desc ^ " else " ^ show_stmt s2.desc
    | SWhile (c, i, ss) ->
        "while " ^ show_cond c.desc ^
        " invariant " ^ show_cond i.desc ^
        " do\n" ^ show_stmt ss.desc ^ "end"
    )

let show_ast ((decls, stmt) : ast) =
    let result = ref "" in
    result := !result ^ "variables: ";
    List.iter (fun x -> result := !result ^ x.desc ^ " ") decls;
    result := !result ^ ";\n";
    result := !result ^ show_stmt stmt;
    !result

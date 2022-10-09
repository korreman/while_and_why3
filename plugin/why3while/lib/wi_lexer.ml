open Why3
open Ptree
open MParser

open Wi_ast

module T = Mparser_tokens

let parse_term _ _ = { term_loc = Loc.dummy_position; term_desc = Ptree.Ttrue; }

let parse_term_list _ _ = [{ term_loc = Loc.dummy_position; term_desc = Ptree.Ttrue; }]

let parse_qualid _ = Ptree.Qident { id_str = ""; id_ats = []; id_loc = Loc.dummy_position }

let parse_list_qualid _ = [ Ptree.Qident { id_str = ""; id_ats = []; id_loc = Loc.dummy_position } ]

let parse_list_ident _ = [{ id_str = ""; id_ats = []; id_loc = Loc.dummy_position } ]

(** primitives **)

type 'a parser = ('a, unit) t
(** generic parser type **)

let pTag (p : 'a parser) : (('a tagged) parser)=
    get_pos >>= fun (_, line_a, col_a) ->
    p >>= fun x ->
    get_pos >>= fun (_, line_b, col_b) -> return ({
        pos = {
            start = { line = line_a; col = col_a };
            stop = { line = line_b; col = col_b };
        };
        desc = x;
    })
(** Convert a parser into one that tags its result with position annotations. **)

let pSymbol s = T.symbol s

(** expressions **)

let pBool : bool parser =
    (pSymbol "true" >>$ true) <|>
    (pSymbol "false" >>$ false)

let pValue =
    pBool >>= fun x -> return (VBool x)

let pVar =
    look_ahead lowercase >> many_chars alphanum

let pTerm =
    (pTag pVar >>= fun x -> return (EVar x)) <|>
    (pTag pValue >>= fun x -> return (EValue x))

let infix (p : 'a parser) (op : binop) (assoc : assoc) : (expr tagged, unit) operator =
    Infix ((pTag p |>> fun p a b ->
        ({pos = { start = a.pos.start; stop = b.pos.stop };
          desc = EBinop ({pos = p.pos; desc = op}, a, b)})),
         assoc)

let operators = [
    [infix (pSymbol "&") Band Assoc_right];
    [infix (pSymbol "|") Bor Assoc_right];
]

let pExpr : expr tagged parser =
    expression operators (pTag pTerm)

let pPred = pExpr

let pInvariant = pPred

(** statements **)

(** NOTE: OCaml doesn't allow the definition of recursive values, thus this helper function. **)
let rec pStmtFn _ =
    let pSkip : stmt parser =
        pSymbol "skip" >>$ SSkip

    in let pAssert : stmt parser =
        pSymbol "assert" >>
        pPred |>> fun p ->
        SAssert p

    in let pAssign : stmt parser =
        pTag pVar >>= fun varName ->
        pSymbol "=" >>
        pExpr |>> fun e ->
        SAssign (varName, e)

    in let pIfElse : stmt parser =
        pSymbol "if" >>
        pExpr >>= fun cond ->
        pSymbol "then" >>
        pTag (pStmtFn ()) >>= fun s1 ->
        pSymbol "else" >>
        pTag (pStmtFn ()) |>> fun s2 ->
        SIfElse (cond, s1, s2)

    in let pWhile : stmt parser =
        pSymbol "while" >>
        pExpr >>= fun cond ->
        pSymbol "invariant" >>
        pInvariant >>= fun invar ->
        pSymbol "do" >>
        pTag (pStmtFn ()) |>> fun s ->
        SWhile (cond, invar, s)

    in choice [pSkip; pAssert; pAssign; pIfElse; pWhile]

let pStmt = pStmtFn ()

(** parser **)

let pAst : ast parser =
    pStmt << pSymbol ";" |> pTag |> many

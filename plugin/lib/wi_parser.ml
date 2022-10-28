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

(*** primitives ***)

type 'a parser = ('a, unit) t
(** generic parser type **)

let pToken (p : 'a parser) : (('a tagged) parser) =
    get_pos >>= fun (_, line_a, col_a) ->
    (p << spaces) >>= fun x ->
    get_pos >>= fun (_, line_b, col_b) -> return ({
        pos = {
            start = { line = line_a; col = col_a };
            stop = { line = line_b; col = col_b };
        };
        desc = x;
    })

(** Convert a parser to a token parser, handling whitespace and tagging positions. **)
(*
    Hmm, still doesn't feel quite right.
    If we replace the contents of a parsed token with a value,
    shouldn't the position information for the originally parsed token still be kept around?
    In other words,
    it'd be best if position information wasn't a part of the result,
    but just stored in the parser.
    Which it is, but we need both the start and stop positions of every token.
    I'm really warming up to the idea of splitting stuff into lexing and parsing for future cases.
*)

let pSymbol s = pToken (T.symbol s)

let pIdent =
    pToken (look_ahead lowercase >> many_chars alphanum)

(*** expressions ***)
(* TODO: Figure out how to not throw out position tags *)

let pBool : bool parser =
    (pSymbol "true" >>$ true) <|>
    (pSymbol "false" >>$ false)

let pValue =
    pBool >>= fun x -> return (VBool x)

let pVar = pIdent

let pTerm =
    (pVar >>= fun x -> return (EVar x)) <|>
    (pToken pValue >>= fun x -> return (EValue x))

let infix (p : 'a parser) (op : binop) (assoc : assoc) : (expr tagged, unit) operator =
    Infix ((pToken p |>> fun p a b ->
        ({pos = { start = a.pos.start; stop = b.pos.stop };
          desc = EBinop ({pos = p.pos; desc = op}, a, b)})),
         assoc)

let operators = [
    [infix (pSymbol "&") Band Assoc_right];
    [infix (pSymbol "|") Bor Assoc_right];
]

let pExpr : expr tagged parser =
    expression operators (pToken pTerm)

let pFormula = pExpr

(** statements **)

(** NOTE: OCaml doesn't allow the definition of recursive values, thus this helper function. **)
let rec pStmtFn _ =
    let pSkip : stmt parser =
        pSymbol "skip" >>$ SSkip

    in let pAssert : stmt parser =
        pSymbol "assert" >>
        pFormula |>> fun p ->
        SAssert p

    in let pAssign : stmt parser =
        pVar >>= fun varName ->
        pSymbol "=" >>
        pExpr |>> fun e ->
        SAssign (varName, e)

    in let pIfElse : stmt parser =
        pSymbol "if" >>
        pExpr >>= fun cond ->
        pSymbol "then" >>
        pToken (pStmtFn ()) >>= fun s1 ->
        pSymbol "else" >>
        pToken (pStmtFn ()) |>> fun s2 ->
        SIfElse (cond, s1, s2)

    in let pWhile : stmt parser =
        pSymbol "while" >>
        pExpr >>= fun cond ->
        pSymbol "invariant" >>
        pFormula >>= fun invar ->
        pSymbol "do" >>
        pToken (pStmtFn ()) |>> fun s ->
        SWhile (cond, invar, s)

    in choice [pSkip; pAssert; pAssign; pIfElse; pWhile]

let pStmt = pStmtFn ()

let pDecls =
    many pIdent << pSymbol ";"

(** parser **)

let pAst : ast parser =
    pDecls >>= fun decls ->
    (pStmt << pSymbol ";" |> pToken |> many) >>= fun body ->
    return (decls, body)

let parse_string s =
    match MParser.parse_string (pAst << eof) s () with
        | Success e -> Result.ok e
        | Failed (msg, _e) -> Result.error msg

let parse file =
    match MParser.parse_channel (pAst << eof) file () with
        | Success e -> Result.ok e
        | Failed (msg, _e) -> Result.error msg

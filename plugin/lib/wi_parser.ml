open Why3
open Ptree
open MParser
open Wi_ast
module T = Mparser_tokens

let parse_term _ _ = { term_loc = Loc.dummy_position; term_desc = Ptree.Ttrue }
let parse_term_list _ _ = [ { term_loc = Loc.dummy_position; term_desc = Ptree.Ttrue } ]
let parse_qualid _ = Ptree.Qident { id_str = ""; id_ats = []; id_loc = Loc.dummy_position }
let parse_list_qualid _ = [ Ptree.Qident { id_str = ""; id_ats = []; id_loc = Loc.dummy_position } ]
let parse_list_ident _ = [ { id_str = ""; id_ats = []; id_loc = Loc.dummy_position } ]

(*** primitives ***)

type 'a parser = ('a, unit) t
(** generic parser type **)

(** Convert a parser to a token parser, handling whitespace and tagging positions. **)
let pToken (p : 'a parser) : 'a tagged parser =
  get_pos >>= fun (_, line_a, col_a) ->
  p << spaces >>= fun x ->
  get_pos >>= fun (_, line_b, col_b) ->
  return
    {
      pos = { start = { line = line_a; col = col_a }; stop = { line = line_b; col = col_b } };
      desc = x;
    }

let pSymbol s = pToken (T.symbol s)

let keywords =
  [ "true"; "false"; "not"; "skip"; "assert"; "if"; "then"; "else"; "while"; "invariant"; "do" ]

let pIdent =
  pToken
    ( look_ahead lowercase >> many_chars alphanum >>= fun ident ->
      if List.exists (fun x -> x == ident) keywords then fail ("reserved keyword: " ^ ident)
      else return ident )

(*** tokens ***)

let pBool : cond parser = pSymbol "true" >>$ FTerm true <|> (pSymbol "false" >>$ FTerm false)
let pInt : expr parser = many1_chars digit |>> fun ds -> EConst (int_of_string ds)
let pVar : expr parser = pIdent |>> fun v -> EVar v.desc

(*** expressions ***)

let pBinop constr (p : 'a parser) op =
  pToken p |>> fun p e1 e2 ->
  {
    desc = constr ({ desc = op; pos = p.pos }, e1, e2);
    pos = { start = e1.pos.start; stop = e2.pos.stop };
  }

let cEBinop (op, e1, e2) = EBinop (op, e1, e2)
let cFBinop (op, c1, c2) = FBinop (op, c1, c2)

let eoperators =
  [
    [
      Infix (pBinop cEBinop (pSymbol "*") BMul, Assoc_right);
      Infix (pBinop cEBinop (pSymbol "/") BDiv, Assoc_left);
      Infix (pBinop cEBinop (pSymbol "%") BRem, Assoc_left);
    ];
    [
      Infix (pBinop cEBinop (pSymbol "+") BAdd, Assoc_right);
      Infix (pBinop cEBinop (pSymbol "-") BSub, Assoc_left);
    ];
  ]

let pExpr : expr tagged parser = expression eoperators (pToken (pInt <|> pVar))

let pNot =
  pSymbol "not" |>> fun symbol c ->
  { desc = FNot c; pos = { start = symbol.pos.start; stop = c.pos.stop } }

let foperators =
  [
    [ Prefix pNot ];
    [ Infix (pBinop cFBinop (pSymbol "/\\") FAnd, Assoc_right) ];
    [ Infix (pBinop cFBinop (pSymbol "\\/") FOr, Assoc_right) ];
    [ Infix (pBinop cFBinop (pSymbol "->") FImplies, Assoc_right) ];
  ]

let pCmp : fcmp parser =
  choice
    [
      pSymbol "=" >>$ CEq;
      pSymbol "<>" >>$ CNe;
      pSymbol ">" >>$ CGt;
      pSymbol ">=" >>$ CGe;
      pSymbol "<" >>$ CLt;
      pSymbol "<=" >>$ CLe;
    ]

let pCompare : cond parser =
  pExpr >>= fun e1 ->
  pToken pCmp >>= fun op ->
  pExpr |>> fun e2 ->
  FCompare (op, e1, e2)

let pCond : cond tagged parser = expression foperators (pToken (attempt pCompare <|> pBool))

(** statements **)

(** NOTE: OCaml doesn't allow the definition of recursive values, thus this helper function. **)
let rec pStmtFn _ =
  let pSkip : stmt parser = pSymbol "skip" >>$ SSkip in

  let pAssert : stmt parser = pSymbol "assert" >> pCond |>> fun p -> SAssert p in

  let pAssign : stmt parser =
    pIdent >>= fun varName ->
    pSymbol "<-" >>
    pExpr |>> fun e ->
    SAssign (varName, e)
  in

  let pIfElse : stmt parser =
    pSymbol "if" >>
    pCond >>= fun cond ->
    pSymbol "then" >>
    pToken (pStmtFn ()) >>= fun s1 ->
    pSymbol "else" >>
    pToken (pStmtFn ()) |>> fun s2 ->
    SIfElse (cond, s1, s2)
  in

  let pWhile : stmt parser =
    pSymbol "while" >>
    pCond >>= fun cond ->
    pSymbol "invariant" >>
    pCond >>= fun invar ->
    pSymbol "do" >>
    many (pToken (pStmtFn ()) << pSymbol ";") >>= fun stmts ->
    pSymbol "end" >>
    return (SWhile (cond, invar, stmts))
  in

  choice (List.map attempt [ pSkip; pAssert; pAssign; pIfElse; pWhile ])

let pDecls = many pIdent << pSymbol ";"
let pStmt = pStmtFn ()

(** parser **)

let pAst : ast parser =
  pDecls >>= fun decls ->
  pStmt << pSymbol ";" |> pToken |> many >>= fun body -> return (decls, body)

let parse_string s =
  match MParser.parse_string (pAst << eof) s () with
  | Success e -> Result.ok e
  | Failed (msg, _e) -> Result.error msg

let parse file =
  match MParser.parse_channel (pAst << eof) file () with
  | Success e -> Result.ok e
  | Failed (msg, _e) -> Result.error msg

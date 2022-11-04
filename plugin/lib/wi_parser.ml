open Why3
open Ptree
open MParser
open Wi_ast
module T = Mparser_tokens

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
  [
    "true";
    "false";
    "not";
    "skip";
    "assert";
    "if";
    "then";
    "else";
    "while";
    "invariant";
    "do";
    "forall";
    "exists";
    "end";
  ]

let pIdent =
  pToken
    ( look_ahead lowercase >> many_chars alphanum >>= fun ident ->
      if List.exists (fun x -> String.equal x ident) keywords then
        fail ("reserved keyword: " ^ ident)
      else return ident )
  <?> "identifier"

(*** tokens ***)

let pInt : expr parser = many1_chars digit |>> (fun ds -> EConst (int_of_string ds)) <?> "integer"
let pVar : expr parser = pIdent |>> (fun v -> EVar v.desc) <?> "variable"

let pBool : cond parser =
  pSymbol "true" >>$ FTerm true <|> (pSymbol "false" >>$ FTerm false) <?> "boolean"

(*** expressions ***)

let pBinop constr (p : 'a parser) op =
  pToken p |>> fun p e1 e2 ->
  {
    desc = constr ({ desc = op; pos = p.pos }, e1, e2);
    pos = { start = e1.pos.start; stop = e2.pos.stop };
  }

let cEBinop (op, e1, e2) = EBinop (op, e1, e2)
let cFBinop (op, c1, c2) = FBinop (op, c1, c2)

(** parse a string that is a prefix of another,
    failing if followed directly by that other string *)
let pPrefix op bad = attempt (pSymbol op << not_followed_by (T.symbol bad) "")

let eoperators =
  let binop p op = pBinop cEBinop p op <?> "integer operator" in
  [
    [
      Infix (binop (pSymbol "*") BMul, Assoc_right);
      Infix (binop (pPrefix "/" "\\") BDiv, Assoc_left);
      Infix (binop (pSymbol "%") BRem, Assoc_left);
    ];
    [
      Infix (binop (pSymbol "+") BAdd, Assoc_right); Infix (binop (pPrefix "-" ">") BSub, Assoc_left);
    ];
  ]

let parens_expression operators terminal =
  let rec parens_term s = (T.parens expr <|> terminal) s
  and expr s = expression operators parens_term s in
  expr

let pExpr : expr tagged parser = parens_expression eoperators (pToken (pInt <|> pVar))

let pNot =
  pSymbol "not" |>> fun symbol c ->
  { desc = FNot c; pos = { start = symbol.pos.start; stop = c.pos.stop } }

let pQuant : fquant parser = choice [ pSymbol "forall" >>$ FForall; pSymbol "exists" >>$ FExists ]

let pQuantCond =
  pToken pQuant >>= fun quant ->
  many1 pIdent >>= fun vars ->
  pSymbol "."
  >> return (fun c ->
         { desc = FQuant (quant, vars, c); pos = { start = quant.pos.start; stop = c.pos.stop } })

let coperators =
  let binop p op = pBinop cFBinop p op <?> "conditional operator" in
  [
    [ Prefix (attempt pNot) ];
    [ Infix (binop (pSymbol "&&") FAnd, Assoc_right) ];
    [ Infix (binop (pSymbol "||") FOr, Assoc_right) ];
  ]

let foperators =
  let binop p op = pBinop cFBinop p op <?> "formula operator" in
  [
    [ Prefix (attempt pNot) ];
    [ Infix (binop (pSymbol "/\\") FAnd, Assoc_right) ];
    [ Infix (binop (pSymbol "\\/") FOr, Assoc_right) ];
    [ Infix (binop (pSymbol "->") FImplies, Assoc_right) ];
    [ Prefix (attempt pQuantCond) ];
  ]

let pCmp : fcmp parser =
  choice
    [
      pSymbol "=" >>$ CEq;
      pPrefix ">" "=" >>$ CGt;
      pSymbol ">=" >>$ CGe;
      pPrefix "<" "=" >>$ CLt;
      pSymbol "<=" >>$ CLe;
    ]

let pCompare : cond parser =
  pExpr >>= fun e1 ->
  pToken pCmp >>= fun op ->
  pExpr |>> fun e2 -> FCompare (op, e1, e2)

let pCond : cond tagged parser = parens_expression coperators (pToken (attempt pCompare <|> pBool))

let pFormula : cond tagged parser =
  parens_expression foperators (pToken (attempt pCompare <|> pBool))

(** statements **)

(** NOTE: OCaml doesn't allow the definition of recursive values, thus this helper function. **)
let rec pStmtFn _ =
  let pLesserStmtFn _ =
    let pSkip : stmt parser = pSymbol "skip" >>$ SSkip in
    let pAssert : stmt parser = pSymbol "assert" >> pFormula |>> fun p -> SAssert p in

    let pAssign : stmt parser =
      pIdent >>= fun varName ->
      pSymbol "<-" >> pExpr |>> fun e -> SAssign (varName, e)
    in

    let pIfElse : stmt parser =
      pSymbol "if" >> pCond >>= fun cond ->
      pSymbol "then" >> pToken (pStmtFn ()) >>= fun s1 ->
      pSymbol "else" >> pToken (pStmtFn ()) >>= fun s2 -> pSymbol "end" >>$ SIfElse (cond, s1, s2)
    in

    let pWhile : stmt parser =
      pSymbol "while" >> pCond >>= fun cond ->
      pSymbol "invariant" >> pFormula >>= fun invar ->
      pSymbol "do" >> pToken (pStmtFn ()) >>= fun stmt ->
      pSymbol "end" >> return (SWhile (cond, invar, stmt))
    in
    choice (List.map attempt [ pSkip; pAssert; pAssign; pIfElse; pWhile ])
  in
  let pSeq = many1 (pToken (pLesserStmtFn () << pSymbol ";")) |>> fun x -> SSeq x in
  attempt pSeq <|> attempt (pLesserStmtFn ())

let pDecls = many pIdent << pSymbol ";"
let pStmt = pStmtFn ()

(** parser **)

let pAst : ast parser =
  pDecls >>= fun decls ->
  pStmt >>= fun body -> return (decls, body)

let parse_string s =
  match MParser.parse_string (pAst << eof) s () with
  | Success e -> Result.ok e
  | Failed (msg, _e) -> Result.error msg

let parse file =
  match MParser.parse_channel (pAst << eof) file () with
  | Success e -> Result.ok e
  | Failed (msg, _e) -> Result.error msg

(** argument parsing **)

let parse_term _ _ = { term_loc = Loc.dummy_position; term_desc = Ptree.Ttrue }
let parse_term_list _ _ = [ { term_loc = Loc.dummy_position; term_desc = Ptree.Ttrue } ]
let parse_qualid _ = Ptree.Qident { id_str = ""; id_ats = []; id_loc = Loc.dummy_position }
let parse_list_qualid _ = [ Ptree.Qident { id_str = ""; id_ats = []; id_loc = Loc.dummy_position } ]
let parse_list_ident _ = [ { id_str = ""; id_ats = []; id_loc = Loc.dummy_position } ]

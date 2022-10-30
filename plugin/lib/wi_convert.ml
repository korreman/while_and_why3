open Why3
open Term
open Wstdlib
open Wi_ast

(*
What is a theory?
How to create a theory?

Base language is Map<String, Theory>:
    Declarations that can be added:
        ty_decl: tysymbol - type declarations
        data_decl: data_decl list - data declarations, probably data type decls??
        param_decl: lsymbol
            ls_name: ident
            ls_args: ty list
            ls_value: ty option
            ls_constr: int
        logic_decl: logic_decl list
            lsymbol, ls_defn
        ind_decl: ind_sign, ind_decl list - inductive predicate declaration
        prop_decl: prop_kind, prsymbol, term - proposition declaration

Wait, these functions are just wrappers for the `create_*_decl` in `core/decl.mli`.
We get actual definitions in there.

and decl_node = private
  | Dtype  of tysymbol          (** abstract types and aliases *)
  | Ddata  of data_decl list    (** recursive algebraic types *)
  | Dparam of lsymbol           (** abstract functions and predicates *)
  | Dlogic of logic_decl list   (** defined functions and predicates (possibly recursively) *)
  | Dind   of ind_list          (** (co)inductive predicates *)
  | Dprop  of prop_decl         (** axiom / lemma / goal *)

There are base theories that can be imported (I think with `use_export`):
    builtin_theory
    bool_theory
    highord_theory
    tuple_theory
This section has two functions named `tuple_theory_name` and `add_decl_with_tuples` as well.

What we want to create is a logic declaration,
for a single main function (and possibly multiple main functions down the line.

Looks like we can create our WP using the term constructors in `core/term.mli`.

I think I can use `t_attr_set` to set positions of terms.
Not sure whether that will actually result in a position, however.
I really hope that I don't have to use the Typing module in order to provide highlights.
As far as I can tell,
that module only creates Ptree instances,
which I'm trying to avoid having to use.
Looking a bit more at attributes,
maybe these are actually the ones used for highlights?
*)

(** convert a location from our AST to a Loc.position *)
let convert_position (p : pos) : Loc.position =
  let file_name = "" in
  let start : Lexing.position =
    {
      pos_fname = file_name;
      pos_lnum = p.start.line;
      pos_bol = 0;
      pos_cnum = p.start.col;
    }
  in
  let stop : Lexing.position =
    {
      pos_fname = file_name;
      pos_lnum = p.stop.line;
      pos_bol = 0;
      pos_cnum = p.stop.col;
    }
  in
  Loc.extract (start, stop)

let rec expr_to_term (f : expr) : term =
  (* TODO: factor out duplication *)
  match f with
  | EConst c -> Term.t_int_const (BigInt.of_int c)
  | EVar v -> create_vsymbol (Ident.id_fresh v) Ty.ty_int |> t_var
  | EBinop (o, f1, f2) ->
      let operation =
        match o.desc with
        | BAdd -> "+"
        | BSub -> "-"
        | BMul -> "*"
        | BDiv -> "/"
        | BRem -> "%"
      in
      let operation =
        create_lsymbol
          (operation |> Ident.op_infix |> Ident.id_fresh)
          [ Ty.ty_int; Ty.ty_int ] (Some Ty.ty_int)
      in
      t_app_infer operation [ expr_to_term f1.desc; expr_to_term f2.desc ]

let rec cond_to_term (c : cond) : term =
  match c with
  | FTerm b -> if b then t_true else t_false
  | FNot c -> t_not (cond_to_term c.desc)
  | FBinop (op, c1, c2) ->
      let op =
        match op.desc with FAnd -> t_and | FOr -> t_or | FImplies -> t_implies
      in
      op (cond_to_term c1.desc) (cond_to_term c2.desc)
  | FCompare (cmp, expr1, expr2) ->
      let cmp_op =
        match cmp.desc with
        (* the API is a bit leaky, so we're forced to define these as strings *)
        | CEq -> "="
        | CNe -> "<>"
        | CGt -> ">"
        | CGe -> ">="
        | CLt -> "<"
        | CLe -> "<="
      in
      let cmp_op =
        create_lsymbol
          (cmp_op |> Ident.op_infix |> Ident.id_fresh)
          [ Ty.ty_int; Ty.ty_int ] (Some Ty.ty_bool)
      in
      t_app_infer cmp_op [ expr_to_term expr1.desc; expr_to_term expr2.desc ]

(** individual statement transformation for weakest precondition calculus *)
let rec wp_stmt (s : stmt) (q : term) : term =
  match s with
  | SSkip -> q
  | SAssert c -> t_and (cond_to_term c.desc) q
  | SAssign (v, expr) ->
      let vs =
        create_vsymbol
          (Ident.id_fresh ~loc:(convert_position v.pos) v.desc)
          Ty.ty_int
      in
      let et = expr_to_term expr.desc in
      t_forall_close [ vs ] [ (*TODO: find out what triggers are*) ]
        (t_implies (t_equ (t_var vs) et) (t_subst_single vs et q))
      (* forall v. v = e -> Q[x <- v] *)
  | SIfElse (c, s1, s2) ->
      t_if (cond_to_term c.desc) (wp_stmt s1.desc q) (wp_stmt s2.desc q)
      (* if e then WP(s1, q) else WP(s2, q) *)
  | SWhile (expr, i, s) -> t_true
(* I /\ forall varr. (I -> if e then WP(s, I) else Q)[warr <- varr]
   where warr are the variables modified the loop body
*)
(* How to retrieve a list of all modified variables?
   It doesn't immediately seem like something you can accomplish with Why3 library functions.
   Some variables may conditionally be modified,
   should those just be treated as always modified?

   How exactly should this one work?
*)

(** weakest precondition calculus *)
let wp (stmts : stmt list) : term = List.fold_right wp_stmt stmts t_true

(** verification condition generator *)
let vc_gen ((vdecls, stmts) : ast) : Theory.theory =
  let psym = Decl.create_prsymbol (Ident.id_fresh "main") in
  let f = wp (List.map (fun stmt -> stmt.desc) stmts) in
  let decl = Decl.create_prop_decl Decl.Pgoal psym f in
  let theory = Theory.create_theory (Ident.id_fresh "some_theory") in
  let theory = Theory.use_export theory Theory.bool_theory in
  let theory = Theory.add_decl theory decl in
  Theory.close_theory theory

(** converts the ast to a theory *)
let convert ast =
  let theory = vc_gen ast in
  let theories = Mstr.empty in
  Mstr.add "main" theory theories

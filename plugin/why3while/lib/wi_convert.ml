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

let convert_position (p: pos) : Loc.position =
    let file_name = "" in
    let start : Lexing.position = {
        pos_fname = file_name;
        pos_lnum = p.start.line;
        pos_bol = 0;
        pos_cnum = p.start.col;
    } in
    let stop: Lexing.position = {
        pos_fname = file_name;
        pos_lnum = p.stop.line;
        pos_bol = 0;
        pos_cnum = p.stop.col;
    } in
    Loc.extract (start, stop)
(** convert a location from our AST to a Loc.position *)

let rec formula_to_term (f: formula) : term =
    match f with
        | EValue { desc = VBool v; _} -> if v then t_true else t_false
        | EVar v ->
            create_vsymbol {
                pre_name = v.desc;
                pre_attrs = Ident.Sattr.empty;
                pre_loc = Some (convert_position v.pos)
            } Ty.ty_bool
            |> t_var
        | ENot f' -> t_not (formula_to_term f'.desc)
        | EBinop (o, f1, f2) ->
            let operation = match o.desc with
                | Band -> t_and_asym
                | Bor -> t_or
            in operation (formula_to_term f1.desc) (formula_to_term f2.desc)

let expr_to_term = formula_to_term

let rec wp_stmt (s: stmt) (q: term) : term =
    match s with
        | SSkip -> q
        | SAssert { desc = f; _ } -> t_and (formula_to_term f) q
        | SAssign (v, e) ->
            let vs = create_vsymbol (Ident.id_fresh ~loc:(convert_position v.pos) v.desc) Ty.ty_bool in
            let e' = expr_to_term e.desc in
            t_forall_close [vs] [(*TODO: find out what triggers are*)] (
                t_implies
                    (t_equ (t_var vs) e')
                    (t_subst_single vs e' q)
            )
            (* forall v. v = e -> Q[x <- v] *)
        | SIfElse (e, s1, s2) -> t_if (expr_to_term e.desc) (wp_stmt s1.desc q) (wp_stmt s2.desc q)
            (* if e then WP(s1, q) else WP(s2, q) *)
        | SWhile (e, i, s) -> t_true
            (*** t_and
                (formula_to_term i.desc)
                () ***)
            (* I /\ forall varr. (I -> if e then WP(s, I) else Q)[warr <- varr]
               where warr are the variables modified the loop body
            *)
(* How to retrieve a list of all modified variables?
   It doesn't immediately seem like something you can accomplish with Why3 library functions.
   Some variables may conditionally be modified,
   should those just be treated as always modified?

   How exactly should this one work?
*)
(** individual statement transformation for weakest precondition calculus *)

let wp (stmts : stmt list) : term =
    List.fold_right wp_stmt stmts t_true
(** weakest precondition calculus *)

let vc_gen ((vdecls, stmts): ast) : Theory.theory =
    let lsym = Term.create_lsymbol
        (Ident.id_fresh "main")
        (List.map (fun _ -> Ty.ty_bool) vdecls)
        None in
    let f = wp (List.map (fun stmt -> stmt.desc) stmts) in
    let ldecl = Decl.make_ls_defn
        lsym
        (List.map
            (fun vdecl -> Term.create_vsymbol
                (Ident.id_fresh ~loc:(convert_position vdecl.pos) vdecl.desc)
                Ty.ty_bool
            )
            vdecls
        )
        f in
    let decl = Decl.create_logic_decl [ldecl] in
    let theory = Theory.create_theory (Ident.id_fresh "some_theory") in
    let theory' = Theory.add_decl theory decl in
    Theory.close_theory theory'

let convert ast =
    let theory = vc_gen ast in
    let theories = Mstr.empty in Mstr.add "main" theory theories

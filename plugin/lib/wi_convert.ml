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

(** create a new, _unique_ variable symbol *)
let mk_vsym s = create_vsymbol (Ident.id_fresh s) Ty.ty_int

(** convert a location from our AST to a Loc.position *)
let mk_pos (p : pos) : Loc.position =
  let file_name = "" in
  let start : Lexing.position =
    { pos_fname = file_name; pos_lnum = p.start.line; pos_bol = 0; pos_cnum = p.start.col }
  in
  let stop : Lexing.position =
    { pos_fname = file_name; pos_lnum = p.stop.line; pos_bol = 0; pos_cnum = p.stop.col }
  in
  Loc.extract (start, stop)

exception Failed_lookup of string

let infix = Ident.op_infix

let rec expr_to_term (ops : lsymbol Mstr.t) vars (f : expr) : term =
  (* TODO: factor out duplication *)
  match f with
  | EConst c -> Term.t_int_const (BigInt.of_int c)
  | EVar v -> ( try Mstr.find v vars |> t_var with Not_found -> raise (Failed_lookup v))
  | EBinop (o, f1, f2) ->
      let operation =
        match o.desc with
        | BAdd -> infix "+"
        | BSub -> infix "-"
        | BMul -> infix "*"
        | BDiv -> "div"
        | BRem -> "mod"
      in
      let operation = Mstr.find operation ops in
      t_app_infer operation [ expr_to_term ops vars f1.desc; expr_to_term ops vars f2.desc ]

let rec cond_to_term (ops : lsymbol Mstr.t) vars (c : cond) : term =
  match c with
  | FTerm b -> if b then t_true else t_false
  | FNot c -> t_not_simp (cond_to_term ops vars c.desc)
  | FBinop (op, c1, c2) ->
      let op =
        match op.desc with FAnd -> t_and_simp | FOr -> t_or_simp | FImplies -> t_implies_simp
      in
      op (cond_to_term ops vars c1.desc) (cond_to_term ops vars c2.desc)
  | FCompare (cmp, expr1, expr2) ->
      let cmp_op =
        match cmp.desc with
        (* the API is a bit leaky, so we're forced to define these as strings *)
        | CEq -> "="
        | CGt -> ">"
        | CGe -> ">="
        | CLt -> "<"
        | CLe -> "<="
      in
      let cmp_op = Mstr.find (Ident.op_infix cmp_op) ops in
      t_app_infer cmp_op [ expr_to_term ops vars expr1.desc; expr_to_term ops vars expr2.desc ]
  | FQuant (q, vs, c) ->
      let quant_f =
        match q.desc with FForall -> t_forall_close_simp | FExists -> t_exists_close_simp
      in
      let vsyms = List.map (fun v -> mk_vsym v.desc) vs in
      let vars' =
        List.fold_left (fun acc (v, var) -> Mstr.add v.desc var acc) vars (List.combine vs vsyms)
      in
      let term = cond_to_term ops vars' c.desc in
      quant_f vsyms [ (*triggers?*) ] term

(** weakest precondition calculus *)
let rec wp ops vars s q =
(*  Pretty.print_term Format.std_formatter q;
  Format.print_string "\n\n";*)
  match s with
  | SSkip -> q
  | SSeq ss -> List.fold_right (wp ops vars) (List.map (fun s -> s.desc) ss) q
  | SAssert c -> t_and_asym_simp (cond_to_term ops vars c.desc) q
  | SAssign (v, e) ->
      let xs = mk_vsym ("_" ^ v.desc) in
      let xt = t_var xs in
      let vs = try Mstr.find v.desc vars with Not_found -> raise (Failed_lookup v.desc) in
      let et = expr_to_term ops vars e.desc in
      t_forall_close_simp [ xs ] [ (*triggers*) ]
        (t_implies_simp (t_equ_simp xt et) (t_subst_single vs xt q))
  | SIfElse (c, s1, s2) ->
      t_if_simp (cond_to_term ops vars c.desc) (wp ops vars s1.desc q)
        (wp ops vars s2.desc q)
  | SWhile (c, i, s) ->
      (* hoooh boy *)
      let c = cond_to_term ops vars c.desc in
      let i = cond_to_term ops vars i.desc in
      let body = wp ops vars s.desc i in
      let imply = t_implies_simp i (t_if_simp c body q) in

      let rec modified_vars s : vsymbol list =
        match s with
        | SSeq ss -> List.map (fun s -> modified_vars s.desc) ss |> List.flatten
        | SAssign (v, _) -> [ Mstr.find v.desc vars ]
        | SWhile (_, _, s) -> modified_vars s.desc
        | SIfElse (_, s1, s2) -> List.append (modified_vars s1.desc) (modified_vars s2.desc)
        | _ -> []
      in
      let vsyms = modified_vars s.desc in
      let new_vsyms = List.map (fun vsym -> "~" ^ vsym.vs_name.id_string |> mk_vsym) vsyms in
      let new_terms = List.map t_var new_vsyms in
      let subst : term Mvs.t = Mvs.of_list (List.combine vsyms new_terms) in

      t_and_simp i (t_forall_close_simp new_vsyms [ (*triggers*) ] (t_subst subst imply))

(** create **)
let mk_vars (ds : decls) : vsymbol Mstr.t =
  List.fold_left
    (fun acc v ->
      let name = v.desc in
      let var = mk_vsym name in
      Mstr.add name var acc)
    Mstr.empty ds

(** verification condition generator *)
let vc_gen env ((vdecls, stmt) : ast) : Theory.theory =
  let int_theory = Env.read_theory env [ "int" ] "Int" in
  let div_theory = Env.read_theory env [ "int" ] "ComputerDivision" in
  let op_symbols = Mstr.set_union int_theory.th_export.ns_ls div_theory.th_export.ns_ls in
  let vars = mk_vars vdecls in
  let f = wp op_symbols vars stmt t_true in
  Pretty.print_term Format.std_formatter f;

  let psym = Decl.create_prsymbol (Ident.id_fresh "main") in
  let decl = Decl.create_prop_decl Decl.Pgoal psym f in

  let theory = Theory.create_theory (Ident.id_fresh "some_theory") in
  let theory = Theory.use_export theory int_theory in
  let theory = Theory.use_export theory div_theory in
  let theory = Theory.add_decl theory decl in
  Theory.close_theory theory

(** converts the ast to a theory *)
let convert ((env, ast) : Env.env * ast) =
  let theory = vc_gen env ast in
  let theories = Mstr.empty in
  Mstr.add "main" theory theories

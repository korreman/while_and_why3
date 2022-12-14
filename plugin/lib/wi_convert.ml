open Why3
open Term
open Wstdlib
open Wi_ast

(** create a new, _unique_ variable symbol *)
let mk_vsym s = create_vsymbol (Ident.id_fresh s) Ty.ty_int

(** convert a location from our AST to a Loc.position *)
let _mk_pos (p : pos) : Loc.position =
  let file_name = "" in
  let start : Lexing.position =
    { pos_fname = file_name; pos_lnum = p.start.line; pos_bol = 0; pos_cnum = p.start.col }
  in
  let stop : Lexing.position =
    { pos_fname = file_name; pos_lnum = p.stop.line; pos_bol = 0; pos_cnum = p.stop.col }
  in
  Loc.extract (start, stop)

exception Undeclared_variable of string

let infix = Ident.op_infix

(* look up and apply a binary operation (arithmetic or comparison) *)
let mk_binop ops name a b =
  let op = Mstr.find name ops in
  t_app_infer op [ a; b ]

(* generate a term from a While-lang expression *)
let rec expr_to_term (ops : lsymbol Mstr.t) vars (f : expr) : term =
  match f with
  | EConst c -> Term.t_int_const (BigInt.of_int c)
  (* lookup existing variable in variable map *)
  | EVar v -> ( try Mstr.find v vars |> t_var with Not_found -> raise (Undeclared_variable v))
  | EBinop (o, f1, f2) ->
      (* use imported operation map to perform arithmetic *)
      let operation =
        match o.desc with
        | BAdd -> infix "+"
        | BSub -> infix "-"
        | BMul -> infix "*"
        | BDiv -> "div"
        | BRem -> "mod"
      in
      mk_binop ops operation (expr_to_term ops vars f1.desc) (expr_to_term ops vars f2.desc)

(* generate a term from a While-lang condition *)
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
      (* use imported operation map to perform comparison *)
      let cmp_op =
        match cmp.desc with CEq -> "=" | CGt -> ">" | CGe -> ">=" | CLt -> "<" | CLe -> "<="
      in
      mk_binop ops (infix cmp_op)
        (expr_to_term ops vars expr1.desc)
        (expr_to_term ops vars expr2.desc)
  | FQuant (q, vs, c) ->
      (* choose a quantifier constructor *)
      let quant_f =
        match q.desc with FForall -> t_forall_close_simp | FExists -> t_exists_close_simp
      in
      (* create new non_colliding symbols for quantified names *)
      let vsyms = List.map (fun v -> mk_vsym v.desc) vs in
      (* extend variable map with newly quantified variables,
         so they can be used in nested terms
      *)
      let vars' =
        List.fold_left (fun acc (v, var) -> Mstr.add v.desc var acc) vars (List.combine vs vsyms)
      in
      let term = cond_to_term ops vars' c.desc in
      quant_f vsyms [ (*triggers*) ] term

(** weakest precondition calculus *)
let rec wp ops vars s q =
  match s with
  | SSkip -> q
  (* run wp in reverse on each statement *)
  | SSeq ss -> List.fold_right (wp ops vars) (List.map (fun s -> s.desc) ss) q
  (* note: asymmetric conjuction *)
  | SAssert c -> t_and_asym_simp (cond_to_term ops vars c.desc) q
  (* we denote the fresh variables created for assignment wp with '_' *)
  | SAssign (v, e) ->
      let xs = mk_vsym ("_" ^ v.desc) in
      let xt = t_var xs in
      let vs = try Mstr.find v.desc vars with Not_found -> raise (Undeclared_variable v.desc) in
      let et = expr_to_term ops vars e.desc in
      t_forall_close_simp [ xs ] [ (*triggers*) ]
        (t_implies_simp (t_equ_simp xt et) (t_subst_single vs xt q))
  | SIfElse (c, s1, s2) ->
      t_if_simp (cond_to_term ops vars c.desc) (wp ops vars s1.desc q) (wp ops vars s2.desc q)
  | SWhile (c, i, s) ->
      (* first, convert components to terms *)
      let c = cond_to_term ops vars c.desc in
      let i = cond_to_term ops vars i.desc in
      let body = wp ops vars s.desc i in

      (* create implication term *)
      let imply = t_implies_simp i (t_if_simp c body q) in

      (* collect all variables that are modified in `s` *)
      let rec modified_vars s : vsymbol list =
        match s with
        | SSeq ss -> List.map (fun s -> modified_vars s.desc) ss |> List.flatten
        | SAssign (v, _) -> [ Mstr.find v.desc vars ]
        | SWhile (_, _, s) -> modified_vars s.desc
        | SIfElse (_, s1, s2) -> List.append (modified_vars s1.desc) (modified_vars s2.desc)
        | _ -> []
      in
      let vsyms = modified_vars s.desc in

      (* create a fresh batch of variables for substituting in place of `vsyms` *)
      (* variables created for while-loops are tagged with '~' *)
      let new_vsyms = List.map (fun vsym -> "~" ^ vsym.vs_name.id_string |> mk_vsym) vsyms in
      let new_terms = List.map t_var new_vsyms in
      let subst : term Mvs.t = Mvs.of_list (List.combine vsyms new_terms) in

      (* construct full formula *)
      t_and_simp i (t_forall_close_simp new_vsyms [ (*triggers*) ] (t_subst subst imply))

(** create fresh variables for every declaration in the list **)
let mk_vars (ds : decls) : vsymbol Mstr.t =
  List.fold_left
    (fun acc v ->
      let name = v.desc in
      let var = mk_vsym name in
      Mstr.add name var acc)
    Mstr.empty ds

(** verification condition generator *)
let vc_gen env ((vdecls, reqs, stmt) : ast) : Theory.theory =
  (* these theories are required in order to perform integer arithmetic *)
  let int_theory = Env.read_theory env [ "int" ] "Int" in
  let div_theory = Env.read_theory env [ "int" ] "ComputerDivision" in
  let op_symbols = Mstr.set_union int_theory.th_export.ns_ls div_theory.th_export.ns_ls in

  (* initialize fresh variables from top declarations *)
  let vars = mk_vars vdecls in
  let assumptions = List.map (fun req -> cond_to_term op_symbols vars req.desc) reqs in

  (* run weakest precondition calculus *)
  let wp_result = wp op_symbols vars stmt t_true in
  let f_assums = t_implies_simp (t_and_asym_l assumptions) wp_result in

  let f_quant = t_forall_close_simp (Mstr.values vars) [] f_assums in
  (*Pretty.print_term Format.std_f ormatter f;*)

  (* turn the resulting predicate into a goal *)
  let psym = Decl.create_prsymbol (Ident.id_fresh "main") in
  let decl = Decl.create_prop_decl Decl.Pgoal psym f_quant in

  let theory = Theory.create_theory (Ident.id_fresh "main_theory") in
  let theory = Theory.use_export theory int_theory in
  let theory = Theory.use_export theory div_theory in
  let theory = Theory.add_decl theory decl in
  Theory.close_theory theory

(** converts the ast to a theory *)
let convert ((env, ast) : Env.env * ast) =
  (* we create just one theory and register it as main *)
  let theory = vc_gen env ast in
  let theories = Mstr.empty in
  Mstr.add "while_main" theory theories

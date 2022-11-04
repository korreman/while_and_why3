open Why3
open Term
open Pretty
open Format

(* print binary logic operations *)
let print_binop fmt = function
  | Tand           -> pp_print_string fmt "\\/"
  | Tor            -> pp_print_string fmt "/\\"
  | Timplies       -> pp_print_string fmt "->"
  | Tiff           -> pp_print_string fmt "<->"

let rec while_ext_printer _x print_any fmt a =
  match a with
  | Pp_term (t, pri) ->
      begin match t.t_node with
        (* TODO: match against individual operators *)
        | Tapp (ls, [t1; t2]) ->
            fprintf fmt (protect_on (pri > 0) "@[%a %a %a@]")
              (while_ext_printer _x print_any) (Pp_term (t1, 0))
              (fun fmt a -> pp_print_string fmt a) (ls.ls_name.id_string)
              (while_ext_printer _x print_any) (Pp_term (t2, 0))
        | Tnot t1 ->
            fprintf fmt (protect_on (pri > 0) "@[not %a@]")
              (while_ext_printer _x print_any) (Pp_term (t1, 1))
        | Tbinop (b, f1, f2) ->
            let p = prio_binop b in
            fprintf fmt (protect_on (pri > p) "@[%a %a@ %a@]")
              (while_ext_printer _x print_any) (Pp_term (f1, (p + 1)))
              print_binop b
              (while_ext_printer _x print_any) (Pp_term (f2, p))
        | _ -> print_any fmt a
      end
  | _ -> print_any fmt a

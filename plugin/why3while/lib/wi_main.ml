open Why3
open Pmodule
open Wstdlib

(* This defines how to parse a While AST from a source file. *)
let file_parser (_env : Env.env) (_path : Env.pathname) (_file : Env.filename) (_c : in_channel) =
    Mstr.empty

(* TODO: Consider writing a function
   that converts our AST to the base language `Theory.theory MStr.t`*)

let () = Env.register_format ~desc:"While format" mlw_language "while" ["wi"] file_parser

(* Alternatively, we can register a new language
   by providing a transformation from our AST to the base language
   (a dictionary of theories).
*)

(* This defines how to pretty-print tasks when working with While.
   For user-facing output, making what is printed consistent with what the user is writing.
*)
let () = Itp_server.add_registered_lang "while" Wi_printer.while_ext_printer

(* This defines ___*)
let () = Args_wrapper.set_argument_parsing_functions "while"
  ~parse_term: Wi_parser.parse_term
  ~parse_term_list: Wi_parser.parse_term_list
  ~parse_qualid: Wi_parser.parse_qualid
  ~parse_list_qualid: Wi_parser.parse_list_qualid
  ~parse_list_ident: Wi_parser.parse_list_ident

(*
    ident: identifier, consists of:
        - a location
        - a string
        - an attribute list, each attribute being either:
            - a position, or
            - a string and integer tag
    qualid: qualified identifier, eg. `Module.SubModule.item` or similar
    term: a logical formula
*)

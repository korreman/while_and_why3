open Why3
open Wstdlib

(* This defines how to parse a While AST from a source file. *)
let file_parser
    (_env : Env.env)
    (_path : Env.pathname)
    (_file : Env.filename)
    (c : in_channel)
    : Wi_ast.ast =
  Result.get_ok (Wi_parser.parse c)

(* Register our while language with a conversion to the base language. *)
let while_language =
  Env.register_language Env.base_language Wi_convert.convert

(* Register the parser for our language. *)
let () = Env.register_format ~desc:"While format" while_language "while" ["wi"] file_parser

(* Register a task printer for user-facing output. *)
let () = Itp_server.add_registered_lang "while" Wi_printer.while_ext_printer

(* Register objects for command parsing (transformations, etc.) *)
let () = Args_wrapper.set_argument_parsing_functions "while"
  ~parse_term: Wi_parser.parse_term
  ~parse_term_list: Wi_parser.parse_term_list
  ~parse_qualid: Wi_parser.parse_qualid
  ~parse_list_qualid: Wi_parser.parse_list_qualid
  ~parse_list_ident: Wi_parser.parse_list_ident

(* Kens gæt: CLI-interaction and IDE.
   Some transformations take arguments,
   and these will be talking about the While syntax.

   User-provided arguments to commands and transformations.
*)

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

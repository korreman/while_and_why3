open Why3
open Pmodule
open Ptree
open Wstdlib

(* This defines how to parse a While AST from a source file. *)
let read_channel env path file c = ()

let () = Env.register_format mlw_language "while" ["wi"] read_channel ~desc:"While format"

(* This defines how to pretty-print tasks when working with While.
   Used for user-facing output, making it consistent with what the user is writing.
*)
let () = Itp_server.add_registered_lang "while" Wi_printer.while_ext_printer

(* *)
let () = Args_wrapper.set_argument_parsing_functions "while"
    Wi_lexer.parse_term
    Wi_lexer.parse_term_list
    Wi_lexer.parse_list_ident
    Lexer.parse_qualid
    Lexer.parse_list_qualid

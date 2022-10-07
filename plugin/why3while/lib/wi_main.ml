open Why3
open Pmodule
open Wstdlib

(* This defines how to parse a While AST from a source file. *)
let read_channel _env _path _file _c = Mstr.empty

let () = Env.register_format mlw_language "while" ["wi"] read_channel ~desc:"While format"

(* This defines how to pretty-print tasks when working with While.
   Used for user-facing output, making it consistent with what the user is writing.
*)
let () = Itp_server.add_registered_lang "while" Wi_printer.while_ext_printer

(* This defines ___*)
let () = Args_wrapper.set_argument_parsing_functions "while"
  ~parse_term: Wi_lexer.parse_term
  ~parse_term_list: Wi_lexer.parse_term_list
  ~parse_qualid: Wi_lexer.parse_qualid
  ~parse_list_qualid: Wi_lexer.parse_list_qualid
  ~parse_list_ident: Wi_lexer.parse_list_ident

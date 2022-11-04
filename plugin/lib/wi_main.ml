open Why3

exception Parse

(* This defines how to parse a While AST from a source file. *)
let file_parser (env : Env.env) (_path : Env.pathname) (_file : Env.filename) (c : in_channel) :
    Env.env * Wi_ast.ast =
  match Wi_parser.parse c with
  | Ok ast ->
        (*print_string (Wi_ast.show_ast ast);
        print_newline ();*)
        (env, ast)
  | Error str ->
        print_string str;
        print_newline ();
        raise Parse

(* Register our while language with a conversion to the base language. *)
let while_language = Env.register_language Env.base_language Wi_convert.convert

(* Register the parser for our language. *)
let () = Env.register_format ~desc:"While format" while_language "while" [ "wi" ] file_parser

(* Register a task printer for user-facing output. *)
let () = Itp_server.add_registered_lang "while" Wi_printer.while_ext_printer

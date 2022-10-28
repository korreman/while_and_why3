# Plugins

The entry-point seems to be found in the bottom of the `mc_main.ml` module:

    let () =
      Env.register_format mlw_language "micro-C" ["c"] read_channel
        ~desc:"micro-C format"

    (* Add an extension of task printing *)
    let () = Itp_server.add_registered_lang "micro-C"
        (fun _ -> Mc_printer.microc_ext_printer)

    (* Add transformation arguments parsing *)
    let () = Args_wrapper.set_argument_parsing_functions "micro-C"
        ~parse_term:(fun _ lb -> Mc_lexer.parse_term lb)
        ~parse_term_list:(fun _ lb -> Mc_lexer.parse_term_list lb)
        ~parse_list_ident:(fun lb -> Mc_lexer.parse_list_ident lb)
        (* TODO for qualids, add a similar funciton *)
        ~parse_qualid:(fun lb -> Lexer.parse_qualid lb)
        ~parse_list_qualid:(fun lb -> Lexer.parse_list_qualid lb)

So the relevant functions are:

- Register a new format. `Env.register_format:`
    - `~desc: Pp.formatted` - Description text
    - `lang: 'a language` - Language definition
    - `fname: fformat` - Format name
    - `extensions: extension list` - Format extensions
    - `parser: 'a format_parser` - Language parser?
- Add task printing. `Itp_server.add_registered_lang:`
    - `lang: string` - Language name
    - `print_ext_any: (Task.task -> any_pp Pp.pp -> any_pp Pp.pp)` -
      Task printing extension
- Register transformation arguments parsing.
  `Args_wrapper.set_argument_parsing_functions:`
    - `fformat` -
    - `parse_term: (Trans.naming_table -> Lexing.lexbuf -> Ptree.term)` -
    - `parse_term_list: (Trans.naming_table -> Lexing.lexbuf -> Ptree.term list)` -
    - `parse_qualid: (Lexing.lexbuf -> Ptree.qualid list)` -
    - `parse_list_qualid: (Lexing.lexbuf -> Ptree.qualid list)` -
    - `parse_list_ident: (Lexing.lexbuf -> Ptree.ident list)` -

A language consists of:

    type 'a language = {
      memo : 'a Hpath.t Wenv.t;
      push : env -> pathname -> 'a -> unit;
      regf : format_info -> unit format_parser -> unit;
      regb : (env -> pathname -> unit) -> unit;
      mutable fmts : unit format_parser Mstr.t;
      mutable bins : (env -> pathname -> unit) list;
      mutable info : format_info list;
    }

Parsers are:
    type 'a format_parser = env -> pathname -> filename -> in_channel -> 'a
Basically, above takes an `in_channel` (file handle) and produces a value.

# Breakdown

The MicroC plugin defines:

- A syntax tree
- A lexer
- A parser
- A task printer

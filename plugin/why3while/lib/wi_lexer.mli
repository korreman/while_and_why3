open Why3

val parse_term: Trans.naming_table -> Lexing.lexbuf -> Ptree.term
val parse_term_list: Trans.naming_table -> Lexing.lexbuf -> Ptree.term list
val parse_qualid: Lexing.lexbuf -> Ptree.qualid
val parse_list_qualid: Lexing.lexbuf -> Ptree.qualid list
val parse_list_ident: Lexing.lexbuf -> Ptree.ident list

val parse: string -> (Wi_ast.ast, string) Result.t

(** Stolen token parsers from MParser,
    as they erroneously require regex support in the library. **)

val symbol: string -> (string, 's) MParser.t
(** [symbol sym] parses the literal string [sym] and returns it. *)

val parens: ('a, 's) MParser.t -> ('a, 's) MParser.t
(** [parens p] parses [p] between parentheses ['('] and [')']. *)

val semi_end1: ('a, 's) MParser.t -> ('a list, 's) MParser.t
(** [semi_sep_end1 p] parses one or more occurrences of [p], separated and
    ended by [';']. It returns a list of the results returned by [p]. *)

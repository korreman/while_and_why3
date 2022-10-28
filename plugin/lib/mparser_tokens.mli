(** Stolen token parsers from MParser,
    as they erroneously require regex support in the library. **)

val symbol: string -> (string, 's) MParser.t
(** [symbol sym] parses the literal string [sym] and returns it. *)

val skip_symbol: string -> (unit, 's) MParser.t
(** [skip_symbol sym] is equivalent to [skip (symbol sym)]. *)

val parens: ('a, 's) MParser.t -> ('a, 's) MParser.t
(** [parens p] parses [p] between parentheses ['('] and [')']. *)

val braces: ('a, 's) MParser.t -> ('a, 's) MParser.t
(** [braces p] parses [p] between curly braces ['{'] and ['}']. *)

val brackets: ('a, 's) MParser.t -> ('a, 's) MParser.t
(** [brackets p] parses [p] between angle brackets ['<'] and ['>']. *)

val squares: ('a, 's) MParser.t -> ('a, 's) MParser.t
(** [squares p] parses [p] between square brackets ['\['] and ['\]']. *)

val semi: (char, 's) MParser.t
(** Parses a semicolon [';']. *)

val comma: (char, 's) MParser.t
(** Parses a comma [',']. *)

val colon: (char, 's) MParser.t
(** Parses a colon [':']. *)

val dot: (char, 's) MParser.t
(** Parses a dot ['.']. *)

val semi_sep: ('a, 's) MParser.t -> ('a list, 's) MParser.t
(** [semi_sep p] parses zero or more occurrences of [p], separated by [';'].
    It returns a list of the results returned by [p]. *)

val semi_sep1: ('a, 's) MParser.t -> ('a list, 's) MParser.t
(** [semi_sep1 p] parses one or more occurrences of [p], separated by [';'].
    It returns a list of the results returned by [p]. *)

val semi_sep_end: ('a, 's) MParser.t -> ('a list, 's) MParser.t
(** [semi_sep_end p] parses zero or more occurrences of [p], separated and
    optionally ended by [';']. It returns a list of the results returned by
    [p]. *)

val semi_sep_end1: ('a, 's) MParser.t -> ('a list, 's) MParser.t
(** [semi_sep_end1 p] parses one or more occurrences of [p], separated and
    optionally ended by [';']. It returns a list of the results returned by
    [p]. *)

val semi_end: ('a, 's) MParser.t -> ('a list, 's) MParser.t
(** [semi_end p] parses zero or more occurrences of [p], separated and ended
    by [';']. It returns a list of the results returned by [p]. *)

val semi_end1: ('a, 's) MParser.t -> ('a list, 's) MParser.t
(** [semi_sep_end1 p] parses one or more occurrences of [p], separated and
    ended by [';']. It returns a list of the results returned by [p]. *)

val comma_sep: ('a, 's) MParser.t -> ('a list, 's) MParser.t
(** [comma_sep p] parses zero or more occurrences of [p], separated by
    [',']. It returns a list of the results returned by [p]. *)

val comma_sep1: ('a, 's) MParser.t -> ('a list, 's) MParser.t
(** [comma_sep1 p] parses one or more occurrences of [p], separated by
    [',']. It returns a list of the results returned by [p]. *)

val char_literal: (char, 's) MParser.t
(** Parses a character literal as defined in the OCaml language and returns
    the character. The literal may contain an escape sequence. *)

val string_literal: (string, 's) MParser.t
(** Parses a string literal as defined in the OCaml language and returns the
    string. The literal may contain escape sequences. *)

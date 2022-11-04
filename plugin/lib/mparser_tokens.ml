open MParser

let symbol s =
  string s << spaces

let char_sp c =
  char c << spaces

let parens p =
  between (char_sp '(') (char_sp ')') p

let semi s =
  char_sp ';' s

let semi_end1 p =
  end_by1 p semi

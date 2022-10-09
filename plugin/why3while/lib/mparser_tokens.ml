open MParser

let symbol s =
  string s << spaces

let skip_symbol s =
  skip_string s << spaces

let char_sp c =
  char c << spaces

let parens p =
  between (char_sp '(') (char_sp ')') p

let braces p =
  between (char_sp '{') (char_sp '}') p

let brackets p =
  between (char_sp '<') (char_sp '>') p

let squares p =
  between (char_sp '[') (char_sp ']') p

let semi s =
  char_sp ';' s

let comma s =
  char_sp ',' s

let colon s =
  char_sp ':' s

let dot s =
  char_sp '.' s

let semi_sep p =
  sep_by p semi

let semi_sep1 p =
  sep_by1 p semi

let semi_sep_end p =
  sep_end_by p semi

let semi_sep_end1 p =
  sep_end_by1 p semi

let semi_end p =
  end_by p semi

let semi_end1 p =
  end_by1 p semi

let comma_sep p =
  sep_by p comma

let comma_sep1 p =
  sep_by1 p comma

let escaped_char s =
  (any_of "nrtb\\\"\'" |>> (function
     | 'n' -> '\n'
     | 'r' -> '\r'
     | 't' -> '\t'
     | 'b' -> '\b'
     | c -> c)) s

let escape_sequence_dec =
  let int_of_dec c = (Char.code c) - (Char.code '0') in
  let char_of_digits d2 d1 d0 =
    char_of_int (100 * (int_of_dec d2) +
                 10 * (int_of_dec d1)
                 + (int_of_dec d0))
  in
  fun s ->
    (digit >>= fun d2 ->
     digit >>= fun d1 ->
     digit >>= fun d0 ->
     try_return3 char_of_digits d2 d1 d0
       "Escape sequence is no valid character code" s) s

let escape_sequence_hex =
  let int_of_hex c =
    match c with
      | '0'..'9' ->
          (Char.code c) - (Char.code '0')
      | 'a'..'f' ->
          (Char.code c) - (Char.code 'a') + 10
      | 'A'..'F' ->
          (Char.code c) - (Char.code 'A') + 10
      | _ ->
          failwith "MParser.int_of_hex: no hex digit"
  in
  let char_of_digits h1 h0 =
    char_of_int (16 * (int_of_hex h1) + (int_of_hex h0))
  in
  fun s ->
    (char 'x' >>
     hex_digit >>= fun h1 ->
     hex_digit >>= fun h0 ->
     try_return2 char_of_digits h1 h0
       "Escape sequence is no valid character code" s) s

let escape_sequence s =
  (escape_sequence_dec
  <|> escape_sequence_hex) s

let char_token s =
  ((char '\\' >> (escaped_char <|> escape_sequence))
  <|> any_char) s

let char_literal s =
  ((char '\'' >> char_token << char_sp '\'')
   <?> "character literal") s

let string_literal s =
  (char '"' >> (many_chars_until char_token (char_sp '"'))
   <?> "string literal") s

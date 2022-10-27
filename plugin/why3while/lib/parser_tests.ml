open Wi_parser
open Wi_ast

let mk_pos (a, b) (c, d) = {
    start = {
        line = a;
        col = b;
    };
    stop = {
        line = c;
        col = d;
    }
}

let%test "skip" =
    match parse_string "x y; skip;" with
        | Result.Ok (decls, [{ pos = p; desc = d }]) ->
            d == SSkip
        | _ -> false

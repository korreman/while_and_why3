open Wi_lexer
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
    match parse "skip;" with
        | Result.Ok [{ pos = p; desc = d }] ->
            d == SSkip
        | _ -> false

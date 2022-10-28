open Why3

val while_ext_printer:
    Task.task ->
    (Format.formatter -> Pretty.any_pp -> unit) ->
    Format.formatter -> Pretty.any_pp ->
    unit

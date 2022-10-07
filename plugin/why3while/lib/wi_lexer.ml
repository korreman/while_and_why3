open Why3
open Ptree

let parse_term _ _ = { term_loc = Loc.dummy_position; term_desc = Ptree.Ttrue; }

let parse_term_list _ _ = [{ term_loc = Loc.dummy_position; term_desc = Ptree.Ttrue; }]

let parse_qualid _ = Ptree.Qident { id_str = ""; id_ats = []; id_loc = Loc.dummy_position }
let parse_list_qualid _ = [ Ptree.Qident { id_str = ""; id_ats = []; id_loc = Loc.dummy_position } ]
let parse_list_ident _ = [{ id_str = ""; id_ats = []; id_loc = Loc.dummy_position } ]

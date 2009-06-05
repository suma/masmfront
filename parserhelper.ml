

(*
type parse_mode =
    Signature
  | Analysis

type procedure = {
  name : string;
  var : (string, int) Hashtbl.t;
}
*)


let lineno = ref 1


let initialize_helper () =
    lineno := 1




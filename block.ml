
open Printf
open Lexer
open Parser
open Type
open Parserhelper

type block = {
  block_name : string;
  masm_code : masm list;
}

type proc = {
  proc_name : string;
  vars : (string * int64) list;
  blocks : block list;
}

let label = ref ""
let inst = ref []
let extern = ref []       (* extern *)
let procs = ref []        (* proc list *)
let code_blocks = ref []  (* block list for instruction *)
let vars = ref []         (* args, local vars info *)

let push_inst c =
  inst := c :: !inst

let init_block () =
  inst := []


let push_var c =
  vars := c :: !vars

let init_var () =
  vars := []

let set_label name =
  label := name

let create_inst_block name =
  let blk = { block_name = name; masm_code = (List.rev !inst) } in
  (*printf "inst block: %d\n" (List.length !inst);*)
  code_blocks := blk :: !code_blocks

let push_block () =
  (*printf "analyze: %s len:%d\n" !label (List.length !inst); flush stdout;*)
  create_inst_block !label;
  init_block ()

let create_proc name =
  let pr = { proc_name = name; vars = (List.rev !vars); blocks = (List.rev !code_blocks) } in
  procs := pr :: !procs


let rec scan_code_segment mlist =
  let rec scan_proc name mlist =
    let scan c =
      match c with
        Label(s) -> printf  "Label: %s\n" s; push_block (); set_label s; ()
      | Instruction _ -> push_inst c
      | InstructionWithPrefix _ -> push_inst c
      | _ -> ()
    in
    match mlist with
      ProcedureEnd(s) :: t when String.compare s name == 0 -> create_proc s; t;
    | a :: t -> scan a; scan_proc name t;
    | _ -> []
  in
  match mlist with
    ProcedureStart(s) :: t -> printf "**Proc: %s\n" s; set_label s; scan_code_segment (scan_proc s t)
  | Let(var, num) :: t -> printf "var: %s\n" var; push_var (var , num); scan_code_segment t
  | SegmentEnd(s) :: t when String.compare s "_TEXT" == 0 -> t
  | _ :: t -> scan_code_segment t
  | _ -> []


let rec scan_all masmlist =
  let push_extern name =
    extern := name :: !extern
  in
  let analyze c =
    match c with
      Label s -> (*printf  "Label: %s\n" s;*) push_block (); label := s; ();
    | Extern(name, _) -> push_extern name
    | _ -> ()
  in
  match masmlist with
    SegmentStart(s) :: t when String.compare s "_TEXT" == 0 -> scan_all (scan_code_segment t);
  | h :: t -> analyze h; scan_all t;
  | _  -> ()


let analyze_code masmlist =
  scan_all masmlist;
  push_block

let main =
  printf "file: %s\n" Sys.argv.(1); flush stdout;
  let file = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in file) in
  let result = Parser.main Lexer.token lexbuf in
    analyze_code result;;
  let oc = open_out_bin (Sys.argv.(1) ^ ".bin") in
    Marshal.to_channel oc !procs [Marshal.No_sharing];
    close_out oc
;;




open Printf
open Lexer
open Parser
open Type
open Parserhelper

type block = {
  block_name : string;
  masm_code : masm list;
}

let label = ref ""
let inst = ref []
let extern = ref []        (* extern *)
let code_blocks = ref []   (* block list for instruction *)

let push_inst c =
  inst := c :: !inst;
;;

let init_block () =
  inst := [];
;;

let rec analyze_dasm_instruction masmlist name =
  let check_function_prologue2 op_push op_mov =
    match op_push, op_mov with
      [[OperandExpression(TermRegister(REG_EBP))]], [[OperandExpression(TermRegister(REG_EBP))]; [OperandExpression(TermRegister(REG_ESP))]] -> true
    | _ -> false
  in
  let check_function_prologue3 op_push op_mov op_sub =
    match op_sub with
      [[OperandExpression(TermRegister(REG_ESP))]; _] when check_function_prologue2 op_push op_mov -> true
    | _ -> false
  in
  match masmlist with
    Instruction(IST_PUSH,op1) :: Instruction(IST_MOV,op2) :: Instruction(IST_SUB,op3) :: next when check_function_prologue3 op1 op2 op3
    -> printf "function var: %s\n" name; flush stdout; (); analyze_dasm_instruction next name; ();
  | Instruction(IST_PUSH,op1) :: Instruction(IST_MOV,op2) :: next when check_function_prologue2 op1 op2
    -> printf "function:     %s\n" name; flush stdout; analyze_dasm_instruction next name;
  | _ :: next -> analyze_dasm_instruction next name; ();
  | _ -> ();
;;

let create_inst_block name =
  let blk = { block_name = name; masm_code = (List.rev !inst) } in
  (*printf "inst block: %d\n" (List.length !inst);*)
  analyze_dasm_instruction blk.masm_code name;
  code_blocks := blk :: !code_blocks;
;;

let push_block () =
  (*printf "analyze: %s len:%d\n" !label (List.length !inst); flush stdout;*)
  create_inst_block !label;
  init_block ();
;;

let rec scan_all masmlist =
  let push_extern name =
    extern := name :: !extern
  in
  let analyze c =
    match c with
      Label s -> (*printf  "Label: %s\n" s;*) push_block (); label := s; ();
    | Instruction _ -> push_inst c
    | InstructionWithPrefix _ -> push_inst c
    | Extern(name, _) -> push_extern name
    | _ -> ()
  in
  match masmlist with
    t :: a -> analyze t; scan_all a;
  | _  -> ()
;;


let analyze_code masmlist =
  scan_all masmlist;
  push_block;
;;

let main =
  printf "file: %s\n" Sys.argv.(1); flush stdout;
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let result = Parser.main Lexer.token lexbuf in
    analyze_code result
;;






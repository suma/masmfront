
open Printf
open Lexer
open Parser
open Type
open Parserhelper

class block (block_name: string) (code: masm list) = object
  val name = block_name
  val masm_code = code
  method get_name = name
  method get_code = masm_code
end


class analyzer = object(self)
  val mutable code_blocks = ([]: block list)
  val mutable label = ""
  val mutable inst  = ([]: masm list)
  val mutable extern = ([]: string list)

  method private push_block =
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
    in
    let code = List.rev inst in
    let blk = new block label code in
    analyze_dasm_instruction code label;
    code_blocks <- blk :: code_blocks;
    inst <- []

  method private push_inst (i: masm) = inst <- i :: inst
  method private push_extern (n: string) = extern <- n :: extern

  method analyze (masmlist: masm list) =
    let rec scan_all masmlist =
      let analyze c =
        match c with
          Label s -> (*printf  "Label: %s\n" s;*) self#push_block; label <- s; ();
        | Instruction _ -> self#push_inst c
        | InstructionWithPrefix _ -> self#push_inst c
        | Extern(name, _) -> self#push_extern name
        | _ -> ()
      in
      match masmlist with
        t :: a -> analyze t; scan_all a;
      | _  -> ()
    in
    scan_all masmlist;
    self#push_block

end


let main =
  printf "file: %s\n" Sys.argv.(1); flush stdout;
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let result = Parser.main Lexer.token lexbuf in
  let ana = new analyzer in
    ana#analyze result
;;






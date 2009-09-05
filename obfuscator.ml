

open Printf
open Lexer
open Parser
open Type
open Parserhelper
open Generator


let rec print_parser masmlist out  =
  let print masmlist out = 
    let str_ir = ir_to_string masmlist in
    if String.compare str_ir "" != 0 then output_string out (sprintf "%s\n" str_ir) else () in
  match masmlist with
    t :: a ->  print t out; print_parser a out
  | [] -> printf "";;

let main =
    (* let lexbuf = Lexing.from_channel stdin in *)
  Printf.printf "file: %s\n" Sys.argv.(1); flush stdout;
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = Parser.main Lexer.token lexbuf in
    if Array.length Sys.argv < 3 then
      print_parser result stdout
    else
      let outfile = open_out Sys.argv.(2) in
        Printf.printf "out: %s\n" Sys.argv.(2);
        print_parser result outfile;
        close_out outfile




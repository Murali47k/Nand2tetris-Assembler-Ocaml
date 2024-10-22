(* main.ml*)
(* opeining all the modules in the main file *)
open Nand2tetris_assembler.Parser
open Nand2tetris_assembler.Ast
open Nand2tetris_assembler.Machine
open Nand2tetris_assembler.Symboltable 

let _ = SymbolTable.init () (*Intializing SymbolTable *)

let f () = 
try read_line () with (* reading line *)
| End_of_file -> "\026" (* End of file character *)

let rec read_till_eof () = (* reading the input till end of file charcter us reached *)
        let input = f () in
        if input = "\026" then ""
        else input ^ "\n" ^ (read_till_eof ())

let () = let input = read_till_eof () in
         let parser_output = tokenize_program input in (* tokenizing the each read line *)
         let translated_output = translate_program parser_output SymbolTable.table in (* translating the read line to what instruction it is *)
         let outputlist = translate_minst_program translated_output in (* converting to machine language *)
         List.iter (fun output ->
                 let string_output = output in
                 print_endline string_output;  (* Print the string output *)
                 ) outputlist (* iterating though the list and printing *)




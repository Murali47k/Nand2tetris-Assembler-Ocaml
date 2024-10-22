(* ast.ml *)
(* opening necessary modules *)
open Symboltable
open Machine

type var = string  (*var : labels and symbols *) 
type address = int (* address for @ instructions *)

(*a record to store a label's name and the address that it corresponds to *)
type label = {name: var;value: address}

(*jump sub instruction in C instruction*)
type jump = NullJump | JGT | JEQ | JGE | JLT | JNE | JLE | JMP

(*destination sub-instruction in C instructions *)
type dest = NullDest | M | D | DM | A | AM | AD | ADM

(* computation sub-instruction in C instructions*)
type comp =
  | Zero | One | MinusOne | Dcomp | Acomp | NotDcomp | NotAcomp | MinusDcomp
  | MinusAcomp | DcompPlus1 | AcompPlus1 | DcompMinus1 | AcompMinus1
  | DPlusA | DMinusA | AMinusD | DAndA | DOrA | Mcomp | NotMcomp
  | MinusMcomp | McompPlus1 | McompMinus1 | DPlusM | DMinusM | MMinusD
  | DAndM | DOrM

(*HACK ASM's Instruction - they are either
label defintintions (Ldef)
C instructions      (Cinst)
At instruction      (At)
A instructions      (Ainst)
 *)
type asminst = 
        Labeldef of var
        | Cinst of dest * comp * jump
        | At of address
        | Ainst of var

(*A program is list of ASM instruction*)
type program = asminst list

(* LABELS *)

(*checks if an instruction is an Label *) 
let is_label (inst : asminst) : bool = 
        match inst with 
        Labeldef _label_name -> true
        | _ -> false

(*gets the label's name from a Label *)
let get_label_name (inst : asminst) : var = 
        match inst with 
        Labeldef label_name -> label_name
        | _ -> ""

(* get_labels_line function for the get_labels function below *)
let rec get_labels_line (prog :program) (current_line :address)=
        match prog with
        [] -> [] 
        | head :: tail ->
                        if is_label head then
                                {name = get_label_name head; value = current_line} :: (get_labels_line tail current_line)
                        else
                                get_labels_line tail (current_line + 1)

(* returns a list of all labels of program this function is intended to be used during the first cycle of the assembly *)
let get_labels (prog : program) = get_labels_line prog 0

(*check if 'var' corresponds to the name of the label in 'labels'
intended to be used to detect which variable is a symbol and which is a label *)

let rec is_in_labels (names :var) (labels :(label list)) = 
        match labels with 
        [] -> false
        | head :: tail ->
                        if head.name = names then true
                        else is_in_labels names tail

(* gets the address corresponding to the label name 'name' among the label list 'labels' *)
let rec get_address_from_lname (name:var) (labels :(label list)) = 
        match labels with
        [] -> 0
        |head :: tail ->
                        if head.name = name then head.value
                        else get_address_from_lname name tail

(* SYMBOLS *)

(*Check if instruction  is an 'Ainst' of a program variable (rather than a symbol) *) 
let is_at_var (inst :asminst) (labels :(label list)) = 
        match inst with 
        Ainst var_name -> if is_in_labels var_name labels then false
                          else true
        | _ -> false

(* gets the variable name *)
let get_var_name (inst : asminst) : var =
        match inst with
        Ainst var_name -> var_name
        | _ -> ""

(*helper function for making a symbol table function
  below to construct the symbol table for the program *)
let rec stable_helper (prog :program) (labels :(label list)) (table) (current_address : address) = 
        match prog with 
        [] -> ()
        |head :: tail ->
                        if is_at_var head labels then
                                let var_name = get_var_name head in
                                if not (SymbolTable.contains_symbol var_name) then
                                        let _unit_ = SymbolTable.add_symbol var_name current_address in 
                                        stable_helper tail labels table (current_address + 1)
                                else
                                        stable_helper tail labels table current_address
                        else
                                stable_helper tail labels table current_address

(* makes a symbol table in a Hash table , considering the labels in label list 'labels'*)
let make_stable (prog :program) (labels :(label list)) table = stable_helper prog labels table 16


(* TRANSLATION *)

(* A- instruction related
           -At instructions : address
           -Ainst instruction : labels and symbols *)

(* returns if n is even function*) 
let even n = if (n-2*(n/2)== 0) then true else false

(* returns if n is odd*)
let odd n = not (even n)

(* Returns ithe binary number corresponding to n *)
let rec dec_to_bin number = 
        if number = 0 then 0
        else if (odd number) then dec_to_bin (number - 1) + 1
        else 10*(dec_to_bin(number/2))

(* python's string zfill function but done in ocaml *)
let rec zfill string n = 
        if (String.length string >= n)
                then (String.sub string ((String.length string) - n) n)
        else 
                "0" ^ (zfill string (n-1))

(* gets the address of an instruction of the form address *)
let get_address (inst :asminst) : address = 
        match inst with 
        At add -> add
        | _ -> 0

(* translate  an address  instruction to it correspondin binary machine language *)
let translate_at (inst :asminst) : minst = 
        let add = get_address inst in
        let bin_add = zfill (string_of_int (dec_to_bin add )) 15 in 
        Minst ("0" ^ bin_add)

(* translate an @ instruction to binary machine language
        inst -> asminst
        labels -> label list 
        symb_table -> (var,address) Hashtbl *)

let translate_ainst (inst : asminst) (labels :(label list)) _symb_table =
        let var_name = get_var_name inst in 
        if is_in_labels var_name labels then
                (* let label = get_label_from_lname var_name labels in *) 
                let label_address = get_address_from_lname var_name labels in 
                        translate_at (At label_address)
        else (* it's in the symbol table *)
                let address = SymbolTable.get_address var_name in 
                        translate_at (At address)

(* C instruction  related 
   C instruction are instruction of the type <destination> = <computation> ; <jump>
   where destination,computation ande jump are each are of type dest,comp and jump , respectively *)

(* making sub instruction for the C instruction *)
let jump_translate (inst :jump) = 
        match inst with 
        NullJump -> "000" | JGT -> "001" | JEQ -> "010" | JGE -> "011"
        | JLT -> "100" | JNE -> "101" | JLE -> "110" | JMP -> "111"

let dest_translate (inst : dest) = 
        match inst with 
        NullDest -> "000" | M -> "001" | D -> "010" | A -> "100"
        | DM -> "011" | AM -> "101" | AD -> "110" | ADM -> "111"

let comp_translate (inst : comp) = 
        match inst with 
        Zero -> "0101010" | One -> "0111111" | MinusOne -> "0111010" | Dcomp -> "0001100" | Acomp -> "0110000"
        | NotDcomp -> "0001101" | NotAcomp -> "0110001" | MinusDcomp -> "0001111" | MinusAcomp -> "0110011"
        | DcompPlus1 -> "0011111" | AcompPlus1 -> "0110111" | DcompMinus1 -> "0001110" | AcompMinus1 -> "0110010"
        | DPlusA -> "0000010"| DMinusA -> "0010011" | AMinusD -> "0000111" | DAndA -> "0000000" | DOrA -> "0010101"
        | Mcomp -> "1110000" | NotMcomp -> "1110001" | MinusMcomp -> "1110011" | McompPlus1 -> "1110111" | McompMinus1 -> "1110010"
        | DPlusM -> "1000010" | DMinusM -> "1010011" | MMinusD -> "1000111" | DAndM -> "1000000" | DOrM -> "1010101"

(* combining all the C instruction *)
let translate_cinst (inst : asminst) =
        match inst with 
        Cinst (dest ,comp , jump) -> Minst ("111"^ (comp_translate comp) ^ (dest_translate dest) ^ (jump_translate jump))
        | _ -> Minst ("")

(* translate_helper function to choose which translation based on its instruction *)
let rec ptranslate_helper ( prog : program ) ( labels : ( label list )) symb_table =
       match prog with 
       [] -> []
       | h :: t -> 
                  let post = ptranslate_helper t labels symb_table in
                  (match h with 
                   Labeldef _name -> post
                   | Cinst (dest,comp,jump) -> translate_cinst ( Cinst (dest,comp,jump)) :: post
                   | At add -> translate_at (At add) :: post
                   | Ainst name -> translate_ainst (Ainst name) labels symb_table :: post)

(* final translate any given instruction *)
let translate_program (prog : program) (preset_table : (var, address) Hashtbl.t) : minst list =
    let labels = get_labels prog in
    let symb_table = preset_table in
    let () = make_stable prog labels symb_table in
    ptranslate_helper prog labels symb_table



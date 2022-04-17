(* Datatypes for assembly instructions *)

open Lang

type 'v simple_expr =
  ConstSE of int
| VarSE of 'v
| BinOpSE of binop * 'v * 'v

(* 'l for labels, 'v for variables *)
type ('l, 'v) instr = 
| Store of 'v * 'v simple_expr  (* store result of simple_expr in variable *)
| Goto of 'l                    (* unconditional goto *)
| Branch of 'v * 'l * 'l        (* if zero then branch to left, else to right *)
| Exit of 'v                    (* return from function leaving result in variable *)


(*index de la variable dans le tableau de variables*)
type instr_index = IIndex of int;;

(* RelJump j jumps j instructions forward (j positive) or backward (j negative) *)
type rel_jump = RelJump of int

(* AbsJump j jumps to absolute address / instruction j *)
type abs_jump = AbsJump of int


(* ************************************************************ *)
(* Operational semantics *)

type exec_result =
  End of int
| Continue of (int array) * int

(* To be defined *)
let string_of_instr_IVar nd = ""


(*calcule la valeur retournee par l'operateur binaire*)
let compute_binop (v1: int) (v2:int) = function 
     BArith(BAadd) -> v1+v2
    |BArith(BAsub) -> v1-v2
    |BArith(BAmul) -> v1*v2
    |BArith(BAdiv) -> if v2=0 then failwith "divised by zero" else v1/v2
    |BArith(BAmod) -> if v2=0 then failwith "divised by zero" else v1 mod v2
    |BCompar(BCeq) -> if v1=v2 then 1 else 0
    |BCompar(BCge) -> if v1>=v2 then 1 else 0
    |BCompar(BCgt) -> if v1>v2 then 1 else 0
    |BCompar(BCle) -> if v1<=v2 then 1 else 0
    |BCompar(BClt) -> if v1<v2 then 1 else 0
    |BCompar(BCne) -> if v1!=v2 then 1 else 0;;


(*calcule la valeur de la simple_exp*)
let compute_expr (varTab : int array) = function 
   ConstSE(c) -> c
  | VarSE(IIndex(i)) -> varTab.(i)
  | BinOpSE(binop,IIndex(i1),IIndex(i2)) -> 
    let v1,v2= varTab.(i1),varTab.(i2) in 
  compute_binop v1 v2 binop ;;

(*Execute l'instruction (rel_jump*instr_index) et return exec_resultat*)
let exec_instr  (instrTab: (rel_jump, instr_index) instr array) 
                    (varTab : int array) (pc: int) =
  (*on execute l'instruction à l'index pc*)
  let the_instr = instrTab.(pc) in 
      match the_instr with

     Goto(RelJump(j)) ->  Continue(varTab,pc+j) 
    |Store(IIndex(i),simple_expr) -> 
      let value = (compute_expr varTab simple_expr) in
        let _  = varTab.(i) <- value in
          Continue(varTab , pc+1)         
    |Branch(IIndex(i),RelJump(j1),RelJump(j2)) -> 
      let v = varTab.(i) in
        if (v=0 )
        then Continue(varTab,j1+pc)   
        else Continue(varTab,j2+pc)     
    |Exit(IIndex(i)) -> End(varTab.(i));;



(*Tant que exec_instr retourne continu on execute la nouvelle instruction
sinon on retourne le résultat*)
let rec run_code (instrTab: (rel_jump, instr_index) instr array) 
             (varTab : int array) (pc: int) = 
    let result = exec_instr instrTab varTab pc in 
      match result with 
   
   End(x) -> x
  |Continue(varT,p) -> run_code instrTab varT p;;






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

type exec_result =
  End of int
| Continue of (int array) * int

(* To be defined *)
let string_of_instr_IVar nd = ""

type instr_var =
  IVarNamed of vname
  |IVarNum of int;;

(*calcule la valeur int retournee par l'operateur binaire*)
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
let exec_instr  (instrTab: (abs_jump, instr_index) instr array) 
                    (varTab : int array) (pc: int) =
  (*on execute l'instruction à l'index pc*)
  let the_instr = instrTab.(pc) in 
      match the_instr with

     Goto(AbsJump(j)) ->  Continue(varTab,j) 
    |Store(IIndex(i),simple_expr) -> 
      let value = (compute_expr varTab simple_expr) in
        let _  = varTab.(i) <- value in
          Continue(varTab , pc+1)         
    |Branch(IIndex(i),AbsJump(j1),AbsJump(j2)) -> 
      let v = varTab.(i) in
        if (v!=0 )
        then Continue(varTab,j1)   
        else Continue(varTab,j2)     
    |Exit(IIndex(i)) -> End(varTab.(i));;



(*Tant que exec_instr retourne continu on execute la nouvelle instruction
sinon on retourne le résultat*)
let rec run_code (instrTab: (abs_jump, instr_index) instr array) 
             (varTab : int array) (pc: int) = 
    let result = exec_instr instrTab varTab pc in 
      match result with 
   
   End(x) -> x
  |Continue(varT,p) -> run_code instrTab varT p;;



(* ************************************************************ *)
(* **** Initialisation Tableau des variables               **** *)
(* ************************************************************ *)
let name_of_vardecl (Vardecl (_, vn)) = vn;;

(*List asso IVarNamed : indice initialisée par parametres + variables locales*)
let rec list_vardecl (i : int) (l : vardecl list) = match l with
    [] -> []
    |(a::b) -> (IVarNamed(name_of_vardecl a),i)::(list_vardecl (i+1) b);; 


(*retourne l'indice le plus élevé d'une variable auxiliaire dans la liste d'instruction*)
let rec max_auxi (m:int) (instrTab: (rel_jump, instr_var) instr list)  = match instrTab with 
    [] -> m
    |(instr::lst) -> match instr with
        Store(IVarNum(i),_) -> let maxi= if i>m then i else m in max_auxi maxi lst
       |Branch(IVarNum(i),_,_) -> let maxi= if i>m then i else m in max_auxi maxi lst
       |Exit(IVarNum(i)) -> let maxi= if i>m then i else m in max_auxi maxi lst
       | _ -> max_auxi m lst;;

(* Retourne la liste d'association IVar_str : *)
let asso_varTab (f: 'a fundefn) (instrTab: (rel_jump, instr_var) instr list) = match f with
    Fundefn(Fundecl(_,_,params),varLocalList,stmt) -> 
        let varTab0= list_vardecl 0 params in 
            let varTab1 = varTab0@(list_vardecl (List.length varTab0) varLocalList) in 
                let nb_var_named= (List.length varTab1) in
                    let nb_var_auxi = max_auxi 0 instrTab in 
                        varTab1@(List.init (nb_var_auxi+1) (fun x -> (IVarNum(x),x+nb_var_named)));;


let init_varTab (f: 'a fundefn) (instrTab: (rel_jump, instr_var) instr list)= 
    let varTab = Array.make (List.length (asso_varTab f instrTab)) 0 in 
        varTab;;
    
let rec init_params (varTab: int array) (i : int) = function 
    [] -> varTab
    |(param::lst) -> let _= Array.set varTab i  param in 
         init_params varTab (i+1) lst;;


(* ************************************************************ *)
(* **** Traduction de RelJump VarNamed to AbsJump IIndex   **** *)
(* ************************************************************ *)


(* Retourne une exp instr_var into IIndex *)
let trad1_exp  (e : 'instr_var simple_expr ) (asso : (instr_var * int) list ) = match e with 
    VarSE(v) -> let index= List.assoc v asso in 
                                VarSE(IIndex(index))
    |BinOpSE(b,v1,v2) -> let index1= List.assoc v1 asso in 
                    let index2= List.assoc v2 asso  in 
                BinOpSE(b,IIndex(index1),IIndex(index2))
    |ConstSE(c) -> ConstSE(c);;


(* Retourne la liste d'instruction IIndex , RelJump*)
let trad1 (f: 'a fundefn) (instrTab: (rel_jump, instr_var) instr list)= 
    let asso = asso_varTab f instrTab in 
        let g (instr : (rel_jump, instr_var) instr ) = match instr with 
            Store(v,a) -> let index= List.assoc v asso in 
                    let new_a = trad1_exp a asso in 
                Store(IIndex(index),new_a)
           |Branch(v,j1,j2) -> let index= List.assoc v asso in 
                Branch(IIndex(index),j1,j2)
           |Exit(v) -> let index= List.assoc v asso in 
                Exit(IIndex(index))
           |Goto(a) -> Goto(a)  in 
                    
                List.map g instrTab ;;

(* Retourne la liste d'instruction IIndex , AbsJump*)
let trad2 (instrTab: (rel_jump, instr_index) instr list) = 
    let g (pc:int) (instr : (rel_jump, instr_index) instr ) = match instr with 
                Branch(v,RelJump(j1),RelJump(j2)) -> Branch(v,AbsJump(j1+pc),AbsJump(j2+pc))
               |Goto(RelJump(j)) -> Goto(AbsJump(j+pc))
               |Exit(v)  -> Exit(v) 
               |Store(v,a) -> Store(v,a)  in 
                   Array.of_list (List.mapi g instrTab);;







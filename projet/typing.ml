(* Typechecking of source programs *)
open Lang;;


(*********************************************************************)
(*                              Tests parsés                         *)
(*********************************************************************)
let test1=Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [],
 Seq
  (Seq
    (Cond (BinOp (0, BCompar BClt, VarE (0, "n"), Const (0, IntV 0)),
      Assign (0, "n",
       BinOp (0, BArith BAsub, Const (0, IntV 0), VarE (0, "n"))),
      Skip),
    While (BinOp (0, BCompar BCgt, VarE (0, "n"), Const (0, IntV 1)),
     Assign (0, "n",
      BinOp (0, BArith BAsub, VarE (0, "n"), Const (0, IntV 2))))),
  Return (BinOp (0, BCompar BCeq, VarE (0, "n"), Const (0, IntV 0)))));;

let test2=Fundefn (Fundecl (IntT, "fac", [Vardecl (IntT, "n")]),
 [Vardecl (IntT, "res")],
 Seq
  (Seq (Assign (0, "res", Const (0, IntV 1)),
    While (BinOp (0, BCompar BCgt, VarE (0, "n"), Const (0, IntV 1)),
     Seq
      (Assign (0, "res",
        BinOp (0, BArith BAmul, VarE (0, "res"), VarE (0, "n"))),
      Assign (0, "n",
       BinOp (0, BArith BAsub, VarE (0, "n"), Const (0, IntV 1)))))),
  Return (VarE (0, "res"))));;


(* Environments *)
type environment = 
    {localvar: (vname * tp) list; 
     returntp: tp};;

let tp_of_expr = function
    Const (t, _) -> t
  | VarE (t, _) -> t
  | BinOp (t, _, _, _) -> t
  | IfThenElse (t, _, _, _) -> t;;

(*********************************************************************)
(*                          Vérification                             *)
(*********************************************************************)

let assign = function (t,v_name,t1,expr,env) -> 
    if (List.exists (function (name,t2 )-> (name=v_name && t=t2)) env) then
        (if (t=t1) then t 
         else (let message = "Expression error" 
            in  failwith message))
    else let message = "Variable not declared" 
            in  failwith message;;
(*
let cond = function (expr,stmt1,stmt2,env) -> 
    let t1,t2=(tp_of_stmt env stmt1) ,(tp_of_stmt env stmt2) in 
        if (t1=t2) then t1
            (*(if (tp_of_expr = BoolT) then t1 
             else failwith "Expression not bool")*)
        else failwith "stmt1 != stmt2";;*)


let rec tp_of_stmt (env:environment) (s : 'a stmt) = match s with 
    Skip -> failwith "Skiped"
  | Assign (t,v_name,expr) ->let t1=(tp_of_expr expr) in assign(t,v_name,t1,expr,env.localvar)
  | Seq (stmt1,stmt2) -> let t1,t2 = (tp_of_stmt env stmt1),(tp_of_stmt env stmt2) in t2
  | Cond (_,stmt1,stmt2) -> let t1,t2 = (tp_of_stmt env stmt1),(tp_of_stmt env stmt2) in if (t1=t2) then t1
                                        else failwith "stmt1 != stmt2"
  | While (_,stmt) -> tp_of_stmt env stmt
  | Return (expr) ->tp_of_expr expr ;; 




let local=[("res",IntT)];;
let returner = IntT;;

let envi1 = {localvar =local; 
     returntp= returner};;

let stmt1= Assign (IntT, "res", Const (IntT, IntV 1));;

tp_of_stmt envi1 stmt1;;

(*********************************************************************)
(*                            Unification                            *)
(*********************************************************************)

type term = Var of string
          |Appl of string * term list;;
(*type term = Var of string | Appl of string * term list*)

(*Retourne vrai si l'element e appartient a la liste*)
let rec exists e= function
    (a::b) when a =e -> true
  |(_::b) -> exists e b
  |[] -> false;;
(*val exists : 'a -> 'a list -> bool = <fun>*)

(*Transforme une liste quelquonque en liste sans doublons*)
let rec into_set = function 
    (a::b) -> if exists a b then into_set b else a::into_set b
  |[]-> [];;
(*val into_set : 'a list -> 'a list = <fun>*)

(*Retourne la liste de toutes les free variables*)
let rec fv_list = function 
    (Var a) -> [Var a]
  |Appl(nom,lst) -> List.concat (List.map fv_list lst);; (*Applique la fonction fv_list a chaque terme de lst et concatene les resultats*)
(*val fv_list : term -> term list = <fun>*)

(*Retourne la liste de toutes les free variables sans doublon*)
let fv = fun t -> into_set (fv_list t);;
(*val fv : term -> term list = <fun>*)

type sub = (string * term) list ;;
(*type sub = (string * term) list*)

let rec subst nom t  = function 
    (Var a) -> if nom = a then t else Var a
  |(Appl(name,lst)) -> Appl(name, List.map (subst nom t) lst);;(*Applique la fonction subst a chaque terme de lst *)
(*val subst : string -> term -> term -> term = <fun>*)                                                       


let rec substituer t (s:sub)= match s with
    ((nom1,t1)::lst) -> subst nom1 t1 (substituer t lst) 
  |[] -> t;;
(*val substituer : term -> sub -> term = <fun>*)


let rec unif (e : (term*term) list) = match e with
    [] -> [] (*E est vide*)
  |((t1,t2)::lst) -> let s = unif lst in
      let simplifier t_1 t_2= match (t_1,t_2) with  (*unification d'une paire*)
        (*DELETE*) (a ,b)  when a=b -> [] 
        (*ORIENT*)              
                 |((Appl(f,lst1) as app),(Var a as e)) -> unif [(e,app)]   
        (*DECOMPOSE*)
                 |(Appl(f,lst1),Appl(g,lst2)) when List.length lst1 = List.length lst2 && f=g-> unif (List.combine lst1 lst2) 
        (*CLASH*)
                 |(Appl(f,lst1),Appl(g,lst2)) ->failwith "Fail : Clash" 
        (*CHECK*)
                 |((Var a as e),t) when exists e (fv t) -> failwith "Fail : Check"
        (*ELIMINATE*)                                                     
                 |(Var a , t) -> [(a,t)]  in 
      (simplifier (substituer t1 s) (substituer t2 s))@s;;




(*********************************************************************)
(*               Conversion type - terme                             *)
(*********************************************************************)


let rec term_of_tp t = match t with 
    VarE(t,name) -> Var(name)
    |Const(t,BoolV v) -> Appl("bool",[])
    |Const(t,IntV v) -> Appl("int",[])
    | BinOp(t,b,e1,e2) -> Appl("binop",(List.map term_of_tp [e1;e2]))
    | IfThenElse(t,e0,e1,e2) -> Appl("IfThenElse",(List.map term_of_tp [e0;e1;e2]));;






(*********************************************************************)
(*                             Inférence                             *)
(*********************************************************************)







(* Compilation functions *)

open Lang
open Instrs

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)



let rec gen_expr (next : int ) (var: instr_var ) (exp : 'a expr)  = match exp with
     Const(_,BoolV(c)) -> let value = if c then 1 else 0 in 
                            [Store(var,ConstSE(value))]
    |Const(_,IntV(c)) -> [Store(var,ConstSE(c))]
    |VarE(_,name) -> [Store(var,VarSE(IVarNamed(name)))]
    |BinOp (_, b, e1,e2) -> 
        let var1,var2 = IVarNum(next),IVarNum(next+1) in
            let ex1,ex2 = gen_expr (next+1) var1 e1 , gen_expr (next+2) var2 e2  in 
                ex1@ex2@[Store(var,BinOpSE(b,var1,var2))]
    |IfThenElse (_,cond, e1,e2) -> 
        let var1 = IVarNum(next) in
             let c,ex1,ex2 = gen_expr (next+1) var1 cond , gen_expr (next) var e1 , gen_expr (next) var e2 in 
                let l1,l2= List.length ex1 ,List.length ex2 in
                    c@[Branch(var1,RelJump(2),RelJump(1))]@[Goto(RelJump(l1+2))]@ex1@[Goto(RelJump(l2+1))]@ex2;;


(*retoure vrai si le statement else possede un return *)
let rec cond_need_goto (instrTab_stmt: (rel_jump, instr_var) instr list) = match instrTab_stmt with
    [] -> true
    |(instr::lst) ->  match instr with 
        |Exit(v)  -> false
        |_ -> true && cond_need_goto lst;;
        

let rec gen_stmt  (stmt: 'a stmt)  = match stmt with 
    Skip -> [Goto(RelJump(1))]
   | Assign(_,name,exp) -> gen_expr 0 (IVarNamed(name)) exp
   | Seq(stmt1,stmt2) -> let s1,s2 = gen_stmt stmt1 , gen_stmt stmt2 in 
          s1@s2
   | While(exp,stmt) -> 
        let e1 = gen_expr 1 (IVarNum(0)) exp in 
            let s1=  gen_stmt stmt  in 
                let l1,l2 = List.length e1,List.length s1 in
                e1@[Branch((IVarNum(0)),RelJump(1),RelJump(l2+2))]@s1@[Goto(RelJump(-(l2+l1+1)))]
  | Cond(exp,stmt1,stmt2) ->
            let e1 = gen_expr 1 (IVarNum(0)) exp  in
                let s1,s2=  gen_stmt stmt1 , gen_stmt stmt2  in  
                    let l1,l2= List.length s1 ,List.length s2 in
                        let need = cond_need_goto s2 in 
                if need 
                then e1@[Branch(IVarNum(0),RelJump(2),RelJump(1))]@[Goto(RelJump(l1+2))]@s1@[Goto(RelJump(l2+1))]@s2
                else e1@[Branch(IVarNum(0),RelJump(2),RelJump(1))]@s1@s2


  | Return(exp) -> let e1 = gen_expr 1 (IVarNum(0)) exp  in 
                e1@[Exit((IVarNum(0)))];;


let gen_instr  (f: 'a fundefn) = match f with
    Fundefn(_,_,stmt) -> 
        gen_stmt stmt ;;



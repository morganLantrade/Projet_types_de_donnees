#load "lang.cmo";;
#load "typing.cmo";;


open Lang;;
open Typing;;



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




let local=[("res",IntT);("n",IntT)];;

let envi1 = {localvar =local;returntp= IntT};;

let expr1= Const (0, IntV 0);;
let expr2= VarE (0, "n");;
let expr3= BinOp (0, BCompar BCgt, VarE (0, "n"), Const (0, IntV 1));;
let expr4= IfThenElse(0,Const (0, BoolV true),Const (0, IntV 0),Const (0, IntV 0));;
let expr4_error1= IfThenElse(0,Const (0, IntV 0),Const (0, IntV 0),Const (0, IntV 0));;
let expr4_error2= IfThenElse(0,Const (0, BoolV true),Const (0, BoolV true),Const (0, IntV 0));;

tp_expr envi1 expr1;;
tp_expr envi1 expr2;;
tp_expr envi1 expr3;;
tp_expr envi1 expr4;;
tp_expr envi1 expr4_error1;;
tp_expr envi1 expr4_error2;;


let stmt1= Assign (0, "n",BinOp (0, BArith BAsub, Const (0, IntV 0), VarE (0, "n")));;
let stmt1_error= Assign (0, "n",Const(0,BoolV true));;

let stmt2=Seq
  (Seq (Assign (0, "res", Const (0, IntV 1)),
    While (BinOp (0, BCompar BCgt, VarE (0, "n"), Const (0, IntV 1)),
     Seq
      (Assign (0, "res",
        BinOp (0, BArith BAmul, VarE (0, "res"), VarE (0, "n"))),
      Assign (0, "n",
       BinOp (0, BArith BAsub, VarE (0, "n"), Const (0, IntV 1)))))),
  Return (VarE (0, "res")));;

tp_stmt envi1 stmt1;;
tp_stmt envi1 stmt1_error;;
tp_stmt envi1 stmt2;;


let envi_test= Fundecl(BoolT, "even", [Vardecl (IntT, "n");Vardecl (IntT, "res")]);;
let envi_test_error= Fundecl(BoolT, "even", [Vardecl (IntT, "n");Vardecl (IntT, "n")]);;
env_of_fundcl envi_test;;
env_of_fundcl envi_test_error;;



tp_fundefn test1;;
tp_fundefn test2;;

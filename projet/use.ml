#load "lang.cmo";;
#load "parser.cmo" ;;
#load "lexer.cmo" ;;
#load "typing.cmo";;
#load "instrs.cmo";;
#load "gen.cmo";;
#load "graph.cmo";;
#load "code2graph.cmo";;
#load "interf.cmo";;
#load "typing.cmo";;

(* For using the parser:

- Evaluate this file (use.ml)
- open Interf ;;
- parse "Tests/even.c" ;;

*)

open Interf;;
open Lang;;
open Typing;;
open Gen;;
open Instrs;;


(********************************)
(*********Paramètres*************)
(********************************)
let param = [];;
let filename = "Tests/test_final.c";;

(**********Parse************)
let code_parse= parse filename;;

(**********Typage************)
let test=tp_fundefn code_parse;;

(****Generation instructions******)
let instructions=gen_instr test;;
let traduct1=trad1 test instructions;;
let traduct2= trad2 traduct1 ;;

(****Generation varTab******)
let varTab= init_varTab test instructions;;
let varTab_init=init_params varTab 0 param;;

(***********************Exécution code********************************)
let test_exec = run_code traduct2 varTab_init 0;;


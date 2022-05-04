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

let run_test (f: 'a fundefn) (params: int list) = 
    let f_typee=tp_fundefn f in 
        let tab_instr0 = Gen.gen_instr f_typee in 
            let tab_instr1 = trad1 f_typee tab_instr0 in
                let tab_instr2= trad2 tab_instr1 in 
                    let varTab= init_varTab f_typee tab_instr0 in
                        let varTab_init=verif_params f_typee params varTab in 
                            run_code tab_instr2 varTab_init 0;;

(********************************)
(*********Paramètres*************)
(********************************)
let params = [0;0;1];;
let filename = "Tests/test_final.c";;
let test= run_test (parse filename) params;;
(*

(******************************************)
(* POUR VISUALISER LES DIFFERENTES ETAPES *)
(******************************************)

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
let varTab_init=verif_params test params varTab ;;

(***********************Exécution code********************************)
let test_exec = run_code traduct2 varTab_init 0;; 

*)





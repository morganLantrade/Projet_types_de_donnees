(* Typechecking of source programs *)
open Lang;;



(* Environments *)
type environment = 
    {localvar: (vname * tp) list; 
     returntp: tp};;

(*********************************************************************)
(*                          Vérification                             *)
(*********************************************************************)

let tp_of_expr = function
    Const (t,_) -> t
  | VarE (t, _) -> t
  | BinOp (t, _, _, _) -> t
  | IfThenElse (t, _, _, _) -> t;;


let tp_const = function
    BoolV(_) -> BoolT
  | IntV(_) -> IntT;;

let tp_binop = function
   BArith(_) -> IntT
  |BCompar(_) -> BoolT;;

(*Retourne un failwith si la variable n'est pas trouvée
Sinon renvoie la tp de la variable dans l'environnement*)
let tp_var env = fun name ->
    let f = fun (v_name,t2)-> (name=v_name) in 
        if (List.exists (f) env.localvar)
        then VarE(snd((List.find f env.localvar)),name)
        else let message = name^" is not declared" in 
            failwith message;;


(*Retourne un expression typée, si y a des incohérences : elle renvoie
un failwith selon l'erreur commise*)
let rec tp_expr (env:environment) (e : 'a expr) = match e with
    Const (_, c) -> Const(tp_const c,c)
  | VarE (_, name) -> tp_var env name 
  |BinOp (_, b, e1,e2) -> 
      let t1,t2 = (tp_expr env e1), (tp_expr env e2) in 
        if (tp_of_expr(t1)=tp_of_expr(t2)) 
        then BinOp((tp_binop b),b,t1,t2)
        else failwith "Binary operator can't be use on different types"
  | IfThenElse (_, cond, e1,e2) -> 
    let t0=tp_expr env cond in 
        if (tp_of_expr t0 = BoolT ) then
            let t1,t2 = (tp_expr env e1), (tp_expr env e2) in 
                if (tp_of_expr t1=tp_of_expr t2) 
                then  IfThenElse(tp_of_expr(t1),t0,t1,t2)
                else failwith "IfThenElse error : different types"
        else failwith "IfThenElse error : cond need to be a boolean";;



(*Retourne l'assignation typée ou l'erreur d'assignation*)
let assign = function (v_name,expr,env) -> 
    let t1,t2= (tp_expr env expr), (tp_var env v_name) in 
        if tp_of_expr t1=tp_of_expr t2 
        then Assign(tp_of_expr t1,v_name,t1) 
        else let message = "Assign expression does not match with variable type "^v_name in
             failwith message;;

(*Retourne le statement typé ou l'erreur commise *)
let rec tp_stmt (env:environment) (s : 'a stmt) = match s with 
    Skip -> Skip
  | Assign (_,v_name,expr) ->assign(v_name,expr,env)
  | Seq (stmt1,stmt2) -> 
        let t1,t2 = (tp_stmt env stmt1),(tp_stmt env stmt2) in Seq(t1,t2)
  | Cond (expr,stmt1,stmt2) -> 
      let t0 = tp_expr env expr in
    if (tp_of_expr t0= BoolT) 
    then let t1,t2 = (tp_stmt env stmt1 ), (tp_stmt env stmt2) in 
      Cond(t0,t1,t2)
    else failwith "Cond error : cond is not a boolean"
  | While (expr,stmt) -> let t0,t1= tp_expr env expr , tp_stmt env stmt  in 
                        While(t0,t1)
  | Return (expr) -> 
    let t = tp_expr env expr in 
        if tp_of_expr t==env.returntp 
        then Return(t)
        else failwith "Return error : the value returned does not match the type of the function" ;; 




(*Prend une declaration de variable et retourne un tuple vname,type*)
let add_env = function 
    Vardecl(t,vname) -> (vname,t);;      

(*Retourne vrai si le nom existe dans la liste d'association l*)          
let exists nom = function
    l -> List.exists (fun(x) -> fst(x)=nom) l;;

(*Retourne vrai si la liste d'association possède deux fois le même nom*)   
let rec doublon = function
    ((name,_)::lst) -> (exists name lst) || (doublon lst)
    |[] -> false;;

(*Retourne l'environnement de fundcl si il n'y a pas d'erreurs*)  
let env_of_fundcl = function
    Fundecl(t,name,list_vardecl) -> 
        let local = List.map add_env list_vardecl in 
            if (doublon local) 
            then failwith "At least 2 parameters declared have the same name"
            else {localvar =local;returntp= t};;

(*Retourne true si le statement a bien un return
retourne false si le statement peut se terminer sans return*)
let rec return_in_stmt (s : 'a stmt) = match s with  
     Seq(stmt1,stmt2) -> (return_in_stmt stmt1) || (return_in_stmt stmt2)
    |While (_,stmt) -> return_in_stmt stmt
    (*si return dans stmt1 il faut un return dans le stmt 2*)
    |Cond(_,stmt1,stmt2) -> (return_in_stmt stmt1) && (return_in_stmt stmt2) 
    |Return(_) -> true
    |_-> false;;


(*Retourne fundefn typée si il n'y a pas d'erreurs sinon failwith*)  
let tp_fundefn = function
    Fundefn(declaration,listeV,stmt) -> 
    (*Créer une liste d'association var_name*tp    *)
    let listeVar = List.map add_env listeV in

    (*error1 : deux variables du même nom sont dans les variables déclarées*)
    let error1 = doublon listeVar in 
        if error1 
        then failwith "At least 2 local variables declared have the same name"
        else let env_local=(env_of_fundcl declaration) in 
         (*error2 : une variable locale à le même nom qu'une variable déclarée*)
         let error2 = doublon (listeVar@env_local.localvar) in 
            if error2 
            then failwith "A local variable have the same name than a parameter"
            else let error3 = not(return_in_stmt stmt) in 
                (*error3 : la fonction peut ne pas return*)
                if error3
                then failwith "The function need to return a value"
                            (*création de l'environnement déclaration+local sans error1&2*)
                else let env = {localvar =env_local.localvar@listeVar;returntp= env_local.returntp} in 
                    (*Vérification du return + stypage de stmt*)
                    Fundefn(declaration,listeV,(tp_stmt env stmt));;


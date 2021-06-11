(* Implementazione DFA *)
type state = int;; (* gli stati sono rappresentati usando gli interi di Ocaml *)
type symbol = char;; (* i simboli insieme con lo stato corrente definiscono lo stato successivo *)
type transition = state * symbol * state;; (* la funzione di transizione è rappresentata come una lista di tuple di Ocaml, che generalizza le coppie *)
type dfa =
  {
    states : state list; (* insieme finito di stati *)
    sigma : symbol list; (* insieme finito di simboli di input, sarebbe l'alfabeto dell'automa *)
    start : state; (* stato di start *)
    transitions : transition list; (* funzione di transizione *)
    accepting : state list; (* insieme di stati accettanti *)
  };;

(* Chinese Wall *)
let noRaW : dfa = 
{
  states = [0;1;2];
  sigma = ['r';'w'];
  start = 0;
  transitions = [
    (0,'r',0);
    (0,'w',1);
    (1,'w',1);
    (1,'r',2);
    (2,'r',2);
    (2,'w',2)
  ];
  accepting = [0;1]
};;

(* Reverse Chinese Wall *)
let noWaR : dfa =
  { states = [0;1;2];
    sigma = ['r';'w'];
    start = 0;
    transitions = [
      (0,'w',0);
      (0,'r',1);
      (1,'r',1);
      (1,'w',2);
      (2,'w',2);
      (2,'r',2)
    ];
    accepting = [0;1]
  };;


(* Funzioni ausiliare *)

(* prende una stringa s, e la converte nei suoi caratteri individuali. *)
let explode s =
  let rec expl i l =
    if i < 0 then l else
      expl (i - 1) (s.[i] :: l) in (* s.[i] returns the ith element of s as a char *)
  expl (String.length s - 1) [];; (* String.length s returns the length of s *)

(* controlla se una lista contiene un elemento *)
let rec contains e l =
  match l with
  | [] -> false
  | hd::tl -> if hd = e then true else contains e tl;;

(* checkAccepts: funzione ausiliaria per la checkAcceptsAll che consente di entrare ad analizzare gli stati accettanti di ciascun automa e vedere se la politica è accettata o meno *)
let checkAccepts str dfa =
  let symbols = explode str in
  let transition state symbol =
    let rec find_state l =
      match l with
      | (s1,sym,s2)::tl ->
          if (s1 = state && symbol = sym) then
            s2 else find_state tl
      | _ -> failwith "Transizione di stato non accettata"
    in find_state dfa.transitions
  in
  let final_state =
    let rec h symbol_list =
      match symbol_list with
      | [hd] -> (transition dfa.start hd)
      | hd::tl -> (transition (h tl) hd)
      | _ -> failwith "Lista dei simboli vuota"
    in
    h (List.rev symbols)
  in
  if (contains final_state dfa.accepting) then
    true
  else
    false

(* naviga su tutta la lista degli automi e definisce per ogni automa se la politica è accettata o meno *)
let rec checkAcceptsAll str dfa_list : bool =
  match dfa_list with
  | [] -> true
  | h::t -> if checkAccepts str h then checkAcceptsAll str t else false

(* Albero di sintassi astratta *) 
type expr =
    | Skip (* operazione corrispondente a una no operation *)
    | CstI of int (* corrisponde al letterale costruttore che trasporta un intero *)
    | CstB of bool
    | Var of string (* corrisponde all'identificatore di variabile *)
    | Let of string * expr * expr (* corrisponde alla introduzione di una nuova variabile *)
    | Letrec of string * string * expr * expr
    | Prim of string * expr * expr	
    | If of expr * expr* expr
    | Fun of string * expr
    | FunR of string * string * expr (* corrisponde alla introduzione ricorsiva di funzioni *)
    | Call of expr * expr
    | Read of string
    | Write of string
    | Open of string
    | CSeq of expr * expr (* corrisponde alla introduzione di una sequenza di operazioni separate da un ; *)
    | Fi of dfa * expr 
    | Policy of string * expr 
    | PolicyUser of (state list * symbol list * state * transition list * state list) * expr (* permette al programmatore di definire la propria policy *)



(* ambiente polimorfico *)
type 'v env = (string * 'v) list;;

 
let rec lookup env x =
  match env with
  | []        -> failwith (x ^ " not found")
  | (y, v)::r -> if x=y then v else lookup r x;;

  
type value =
    | Int of int
    | Bool of bool 
    | Closure of string * expr
    (* string: nome del parametro formale, expr: codice della funzione + ambiente al momento della dichiarazione*)
    | RecClosure of string * string * expr
    (* string: nome della funzione, string: nome del parametro formale, expr: codice della funzione + ambiente al momento della dichiarazione *)
;;

(* Interprete *) 
let rec eval expr (env: value env) (eta:string) (dfa_list:dfa list) =
  match expr with 
  | Skip -> (Int(0), env, eta)
  | CstI (n)-> (Int(n), env, eta)
  | CstB (n)-> if n then (Int(1), env, eta) else (Int(0), env, eta) 
  | Var (x) -> ((lookup env x), env, eta)
  | Prim(op,e1,e2) ->
      let v1, env, eta = eval e1 env eta dfa_list in
      let v2, env, eta = eval e2 env eta dfa_list in
      begin 
        match (op, v1, v2) with
        | ("*", Int i1, Int i2) -> (Int (i1 * i2), env, eta)
        | ("+", Int i1, Int i2) -> (Int (i1 + i2), env, eta)
		| ("-", Int i1, Int i2) -> (Int (i1 - i2),env, eta)
		| ("=", Int i1, Int i2) -> (Int(if i1 = i2 then 1 else 0),env, eta)
        | (_,_,_) -> failwith("Undefined primitive")
      end
  | If(e1, e2, e3) ->
      begin
        match eval e1 env eta dfa_list with
        | (Int 0,_,eta) -> eval e3 env eta dfa_list
        | (Int _,_,eta) -> eval e2 env eta dfa_list
        | _     -> failwith("Error in the if statement")
      end	
  | Let(s,e1,e2) -> let xval, env, eta = eval e1 env eta dfa_list in
      let env = (s,xval)::env in
      eval e2 env eta dfa_list 
  | Fun(s,expr) -> (Closure (s, expr), env, eta)
  | FunR(f, i, fBody) -> (RecClosure (f,i,fBody), env, eta);
  | Call(e1, e2) -> let f, env, eta = eval e1 env eta dfa_list in
		begin
			match f with
				| Closure(p,e) ->
					let (xval, env, eta) = eval e2 env eta dfa_list in
					let env1 = (p,xval)::env in
						eval e env1 eta dfa_list
				| RecClosure(n, p, e3) ->
					let (aVal,env,a2) = eval e2 env eta dfa_list in
					let rEnv =(n,f)::env in
					let aenv =(p,aVal)::rEnv in
						eval e3 aenv eta dfa_list
				| _ -> failwith("Error during call: is not a function")				
		end
  | Letrec(f, i, fBody, bodyLet) ->
				let (rval,_,a1) = eval (FunR(f, i, fBody)) env eta dfa_list in
				let bindEnv = (f,rval)::env in
				eval bodyLet bindEnv eta dfa_list
				
  | Read(s) -> let eta_new = eta^"r" in
      if (checkAcceptsAll eta_new dfa_list) then (Int (1), env, eta_new)
      else failwith "Read not permitted"
  | Write(s) -> let eta_new = eta^"w" in
      if (checkAcceptsAll eta_new dfa_list) then (Int (2), env, eta_new)
      else failwith "Write not permitted"
  | Open(s) -> let eta_new = eta^"o" in
      if (checkAcceptsAll eta_new dfa_list) then (Int (3), env, eta_new)
      else failwith "Open not permitted"
  | CSeq(e1, e2) -> let tmp, env, eta = eval e1 env eta dfa_list in
      eval e2 env eta dfa_list
  | Fi(dfa, e) -> eval e env eta (dfa::dfa_list) (* istanzia una nuova policy aggiungendola alla dfa_list *)
  | Policy (s, e) -> (* policy definite staticamente *)
      begin
        match s with
        | "noRaW" -> eval e env eta (noRaW::dfa_list)
        | "noWaR" -> eval e env eta (noWaR::dfa_list)
        | _ -> failwith "e1"
      end
  | PolicyUser(s1,e) -> (* policy definite dall'utente *)
			match s1 with
			| (t,s,z,v,y) -> let automa_policy =
                                                    {
                                                    states = t;
                                                    sigma = s;
                                                    start = z;
                                                    transitions = v;
                                                    accepting = y
                                                    } in
                                                        eval e env eta (automa_policy::dfa_list)
            | _ -> failwith "is not an automa policy"
				;;

(*
eval (Letrec("fact", "n",If
		(Prim("=",Var("n"),CstI(0)),CstI(1),
			Prim("*",Var("n"),Call(Var("fact"),
			Prim("-",Var("n"),CstI(1))))),
			Call(Var("fact"),CstI(3)))) [] "" [];;
*)
(***** TEST CASE NO WRITE AFTER READ CON DEFINIZIONE DI UNA POLICY DA PARTE DELL'UTENTE ******)

eval (
      CSeq( Write("Yousay"),
                              PolicyUser( (*automa definito dal programmatore - NoWaR*)
                                          (
                                            [0;1;2], (*stati*)
                                            ['r';'w'], (*sigma*)
                                            0, (*initial state*)
                                            [(0,'w',0);(0,'r',1);(1,'r',1);(1,'w',2);(2,'w',2);(2,'r',2)], (*transizioni*)
                                            [0;1] (*stati accettanti*)
                                          ),
                                          CSeq( Read("Hello"), CSeq(Write("I saygoodbye"), Skip ) )
                              )
          )
) [] "" [];;

(***** TEST CASE NO WRITE AFTER READ ******)
(*eval (CSeq(Policy("noWaR", Read("mondo")),Write("Ciao")) ) [] "" [];;*)

(***** TEST CASE NO WRITE AFTER READ CON OPERAZIONE DI READ PRIMA DELLA DEFINIZIONE DELLA POLICY ******)
(*eval (CSeq(Read("Ciao"),Policy("noWaR", Write("mondo") ) ) ) [] "" [];;*)

(***** TEST CASE NO READ AFTER WRITE CON READ DEFINITA DOPO LA WRITE *****)
(*eval ( CSeq( Write("bella"), Fi( noRaW, Read("Bella") ) ) ) [] "" [];;*)

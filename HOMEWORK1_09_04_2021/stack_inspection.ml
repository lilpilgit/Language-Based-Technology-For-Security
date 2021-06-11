exception DynamicTypeError of string (* eccezione per errore di tipo a run time *)

(* i tipi di permessi assegnabili a una funzione *)
type tpermission =
  | Pread
  | Pwrite;;
 
 (* il tipo per gli identificatori di variabili, funzioni etc... *)
 type ide = string;;

(*sec manager: la struttura usata a run time per memorizzare i permessi di tutte le funzioni che sono state chiamate *)
type permList = (tpermission list) list;;

(* albero di sintassi astratta *)
type expr =
  | CstI of int
  | CstB of bool
  | Var of ide
  | Let of ide * expr * expr
  | Prim of string * expr * expr	
  | IfThenElse of expr * expr * expr 	
  (* corrisponde alla introduzione di una funzione: nome argomento, corpo, lista di permessi *)
  | Fun of ide * expr * tpermission list 
  | Call of expr * expr
  | Read of string
  | Write of string
  | Open of string;;

(* ambiente polimorfico *)
type 'v env = (string * 'v) list;;
  
type evT =
  | Int of int
  | Bool of bool
  | String of string
  (* ide: nome del parametro formale, expr: codice della funzione, evT env ambiente al momento della dichiarazione,
    tpermission list: lista di permessi assegnati alla funzione *)
  | Closure of ide * expr * evT env * tpermission list;;

let bind (env: evT env) (i: ide) (v: evT) = ( i, v ) :: env;;

let rec lookup env x =
  match env with
  | []        -> failwith (x ^ " not found")
  | (y, v)::r -> if x=y then v else lookup r x;;
 
(*support functions*)

(*
  sono state introdotte queste 3 funzioni perchè il sec_manager è la struttura usata a run time
  per gestire la stack inspection e pensato come una lista di liste di permessi. Dunque si parte da una lista vuota,
  inizialmente a static time nessuno chiama nessuno, a run time quando una funzione viene invocata aggiungiamo la lista
  dei suoi permessi alla lista delle liste, il sec_manager. Questo stack continua a crescere fin tanto che non viene fatta
  la demand permission quando viene chiamata la primitiva di Read, Write, etc..
*)

let rec verify_singlePermission (pSraw:tpermission list) (pDem:tpermission) : bool =
  match pSraw with
  | [] -> false (* se arriviamo in fondo alla lista significa che non abbiamo trovato il permesso pDem *)
  | x::r1 -> if  x = pDem then true else verify_singlePermission r1 pDem;;

(*
  la demandPermission accetta come parametri:
  permStack: la lista di permessi della singola funzione che stiamo analizzando 
  demanded: la lista di permessi da verificare, appartenenti alla funzione che ha chiamato una funzione privilegiata. 
  Si occupa di verificare se tutto ciò che è presente nella demanded è nella lista estratta dallo stack.
  Dunque per ogni elemento in demanded viene chiamata la funzione verify_singlePermission.
  PER OTTENERE IL PERMESSO, OGNI ELEMENTO IN DEMANDED DEVE ESSERE IN OGNI LISTA DI PERMSTACk
*)
let rec demandPermission (permStack:tpermission list) (demanded:tpermission list) : bool =
		match demanded with
		| [] -> true (* lasciato per motivi di performance nel caso di una lista di permessi vuota *)
		| x::y -> match permStack with (* x è una lista quindi... *)
              | [] -> false
              | z::u -> if verify_singlePermission permStack x then demandPermission permStack y else false;;

(* 
  Questa funzione si occupa di fare il controllo dei privilegi. Accetta come parametro:
  permStack: una lista di liste di permessi che rappresenta i vari chiamanti ognuno con i propri permessi
  demanded: la lista di permessi che vogliamo verificare
  Viene verificato che ognuno dei demanded è presente all'interno del permStack, ma questo viene fatto solo dopo che
  con questa funzione si è staccato un record di lista dallo stack di tutte le liste, ovvero dal permStack.
  Se tutte le liste di liste di permessi hanno quella lista di permessi presentati dalla funzione che chiama
  l'operazione privilegiata, allora la stack inspection ha successo e la funzione può eseguire la funzione privilegiata.
*)
let rec superPrivilegesVerifier (permStack:permList) (demanded:tpermission list) : bool =
  match demanded with
  | [] -> true
  | _  -> match permStack with
          | [] -> true
          | x :: y -> demandPermission x demanded && superPrivilegesVerifier y demanded;;




(* 
let rec SuperPrivilegesVerifier (permStack:permList) (demanded:tpermission list) :bool =
		match permStack with
		| [] -> true
		| x::y -> if demandPermission x demanded then 
        begin
          match demanded with
          | [] -> true
          | h::t -> SuperPrivilegesVerifier y t 
        end
      else false

*)                  


(*Interprete*)
(*
  Nell'interprete ci portiamo dietro sia l'ambiente che le chiamate a funzioni a run time
*)

let rec eval (expr:expr) (env:evT env) (sec_manager:permList) : evT =
  match expr with
  | CstI (n)-> Int (n)
  | CstB (n)-> if n then Int(1) else Int(0) 
  | Var (x) -> (lookup env x)
  | Prim (op,e1,e2) -> 
      let v1 = eval e1 env sec_manager in
      let v2 = eval e2 env sec_manager in
      begin
        match (op, v1, v2) with
          | ("*", Int i1, Int i2) -> Int (i1 * i2)
          | ("+", Int i1, Int i2) -> Int (i1 + i2)
          | (_,_,_) -> failwith("unexpected")
      end
  | IfThenElse (guardia, e1, e2) -> (* corrisponde alla valutazione di un if then else *)
                                  (
                                      let evalCond = eval guardia env sec_manager in (* valuto l'espressione della guardia... *)
                                          match evalCond with 
                                          | Bool(true) -> eval e1 env sec_manager (* se la guardia è booleana e il valore del ramo if è true, valuto il ramo if *)
                                          | Bool(false) -> eval e2 env sec_manager (* se la guardia è booleana e il valore del ramo if è false, valuto il ramo else *)
                                          | _ -> raise (DynamicTypeError "La guardia dell' ifthenelse deve essere di tipo booleano") 
                                  )
  | Let (s,e1,e2) -> let xval = eval e1 env sec_manager in
      let env1 = bind env s xval in 
      eval e2 env1 sec_manager 			
(*
  Facciamo il controllo dei privilegi ogni qual volta si fa un azione privilegiata, quindi ogni volta che facciamo una read,
  una write o una open chiamiamo la funzione superPrivilegesVerifier che i permessi siano di Pread, Pwrite o entrambi. Se così
  non fosse falliamo. 
*)					
  | Read(s) -> if superPrivilegesVerifier sec_manager [Pread] then String("chiamata la read con successo") else failwith("Errore permessi read")
  | Write(s) -> if superPrivilegesVerifier sec_manager [Pwrite] then String("chiamata la write con successo") else failwith("Errore permessi write")
  (* la Open è stata introdotta solo per testare il caso di operazione privilegiata con più permessi *)
  | Open(s) -> if superPrivilegesVerifier sec_manager [Pwrite;Pread] then String("chiamata la open con successo") else failwith("Errore permessi open")

  (* 
    in fase di dichiarazione vengono aggiunti solamente i privilegi nella chiusura nella forma di lista,
    dunque i permessi per ogni funzione verranno definiti come una lista di permessi [Pread;Pwrite;...]  
  *)           
                                                                               
  | Fun(arg,body,permissions) -> Closure (arg,body, env, permissions)
  | Call(e1, e2) ->
    (* 
      il sec_manager che viene usato nella eval della espressione e1 (potrebbe essere una espressione
      della sintassi astratta) sarebbe [] passato alla eval come secondo parametro di volta in volta
      usato come stack per contenere tutti i permessi di tutte le funzioni
    *)
     let f = eval e1 env sec_manager in 
        match f with
        | Closure(arg,body,fdecEnv,permissions) -> 
    		(* i privileges sono i permessi attuali della funzione *)
(*
  metterlo nella dichiarazione ha il problema che se una funzione viene dichiarata ma non viene chiamata i suoi privilegi vengono
  messi lo stesso sul security manager e quindi influiscono con i permessi delle altre funzioni che poi vengono invece chiamate
  Dato che una funzione quando fa la call il suo record di attivazione viene messo sullo stack solo al momento della call, molto
  probabilmente deve avvenire nella call l'aggiunta del security manager però si ha il problema di sotto dell'esempio con le
  funzioni innestate in cui viene valutata prima quella più interna e poi quella più esterna. Abbiamo deciso di fare delle primitive
  già pronte che usano la stack inspection è più controllabile perchè lasciare i permessi direttamente via codice non è facile.
*)

            (* sec_manager è quello che nelle funzioni ausiliarie è stato chiamato permStack *)
            let actualVal = eval e2 env sec_manager  in
            let sec_manager_extended = permissions::sec_manager in 
            (*
              i permessi vengono salvati sulla struttura a run time che si occupa di memorizzarli poi, quando ci sarà
              una chiamata ad una operazione che richiede la stack inspection viene fatto il controllo (vedere Read, Open e Write)
            *)
            let actualEnv = bind fdecEnv arg actualVal in
            eval body actualEnv sec_manager_extended
        | _ -> failwith("errore non e' una closure")
;;

(* ======================================================================================================================= *)

(*
Example of nested function. Corresponding Ocaml code is:
Si tratta di una funzione che crea un altra funzione al suo interno e la richiama
let f = fun x ->
  let g = fun y -> write() in
    g (x + 1)
  in f 10
*)

(*
  Questa eval ha successo: il chiamante ha il permesso di read, 
  il chiamato ha il permesso di write e read
  Viene chiamata f con parametro 10 ed f al suo interno ha la funzione
  g che viene richiamata una volta sola.
*)
eval(                 
  Let(
    "f",
    Fun(
      "x",
      Let(
        "g",
        Fun(
          "y",
          Read("prova"),
          [Pwrite;Pread]
        ),
        Call(
          Var "g",
          CstI(1)
        )
      ),
      [Pread] (* !!!!!!!!! TOGLIENDO QUESTO PERMESSO ALLA FUNZIONE PIÙ ESTERNA SOLLEVA UN ECCEZIONE !!!!!!!!! *)
    ),
    Call(
      Var "f", CstI(10)
    )
  )
) [] [];;

(* ======================================================================================================================= *)

(*
  eval(        (*Questa eval fallisce: il chiamante ha il permesso di read, il chiamato ha il permesso di write e read e cerca di eseguire una open che richiede entrambi i permessi di write e read*)
    Let(
      "f",
      Fun(
        "x",
        Let(
          "g",
          Fun(
            "y",
            Open("prova"),
            [Pwrite;Pread]
          ),
          Call(
            Var "g",
            CstI(2)
          )
        ),
        [Pread]
      ),
      Call(
        Var "f", CstI(10)
      )
    )
  ) [] [];;
*)

(* ======================================================================================================================= *)

(*
  eval(
    Let(
      "f",
      Fun(
        "x",
        Let(
          "g",
          Fun(
            "y",
            Open("prova"),
            [Pread]
          ),
          Call(
            Var "g",
            CstI(3)
          )
        ),
        [Pread;Pwrite]
      ),
      Call(
        Var "f", CstI(10)
      )
    )
  ) [] [];;
*)

(* ======================================================================================================================= *)

(* test con una singola funzione *)
(*
  eval (
    Call (
      Fun ("x",Write("prova"),[Pread]),
      CstI(4)
    )
  ) [] [];;
*)

(* ======================================================================================================================= *)
(*
test effettuato con 3 funzioni con 3 permessi diversi 
  let f = fun x ->
    let g = fun y -> 
      let h = fun z -> write() in
      h x in g 5
  in f 10
*)

eval(        
  Let(
    "f",
    Fun(
      "x",
      Let(
        "g",
        Fun(
          "y",
          Let(
            "h",
            Fun(
              "z",
              Open("Hello world!"),
              [Pread;Pwrite]
            ),
            Call(
              Var "h",
              Var "x"
            )
          ),
          [Pread;Pwrite]
        ),
        Call(
          Var "g",
          CstI(5)
        )
      ),
      [Pread;Pwrite]
    ),
    Call(
      Var "f", CstI(10)
    )
  )
) [] [];;  
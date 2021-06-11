exception DynamicTypeError of string (* eccezione per errore di tipo a run time *)
exception SetIsEmpty of string (* eccezione per insieme vuoto a run time *)


type ide = string;; (* per gli identificatori delle variabili *)

type ttype = 
    | IntType
    | StringType
    | BoolType
    | FunType of ttype * ttype
    | PermissionType of tpermission
    | SetPermissionsType of tpermission (* tipo usato per l'insieme dei permessi associato a una funzione *)
    and tpermission = 
    | Empty (* permesso vuoto implica un deny any di default*)
    | PermAC of string * string;;

(* albero di sintassi astratta del linguaggio *)
type expr =
    | CstTrue (* corrisponde al letterale true *)
    | CstFalse (* corrisponde al letterale false *)
    | CstInt of int (* corrisponde al letterale costruttore che trasporta un intero *)
    | CstString of string (* corrisponde al letterale costruttore che trasporta una stringa *)
    | EqualsString of expr * expr (* corrisponde all'uguaglianza tra stringhe *)
    | Den of ide (* corrisponde all'identificatore di variabile *)
    | Add of expr * expr (* corrisponde alla somma tra interi *)
    | Sub of expr * expr (* corrisponde alla differenza tra interi *)
    | Times of expr * expr (* corrisponde al prodotto tra interi *)
    | IsZero of expr (* corrisponde a verificare se un intero ha valore 0 *)
    | Equals of expr * expr (* corrisponde a verificare l'uguaglianza tra 2 espressioni intere *)
    | And of expr * expr (* corrisponde all'and tra due espressioni *)
    | Or of expr * expr (* corrisponde all'or tra due espressioni *)
    | Not of expr (* corrisponde alla negazione di un espressione *)
    | IfThenElse of expr * expr * expr (* corrisponde alla valutazione di un if then else *)
    | Let of ide * expr * expr * expr (* corrisponde alla introduzione di una nuova espressione o variabile *)
    (* sia in Fun che FunRec l'argomento verrà passato tramite la Apply, che invece corrisponde alla applicazione di funzione *)
    | Fun of ide * expr * ttype * ttype * expr (* corrisponde alla introduzione di una funzione: nome argomento, corpo, tipo argomento, tipo del ritorno *)
    | FunRec of ide * ide * ttype * ttype * expr * expr * expr (* corrisponde alla introduzione ricorsiva di funzioni *)
    | Apply of expr * expr (* applicazione di una funzione *)
    (* Costruttore per un nuovo permesso *)
    | NewPermission of expr * expr (* prende due espressioni in quanto si potrebbe decidere di definire i permission in modo dinamico con if else etc... *)
    (* Costruttori dell'insieme dei permessi *)
    | EmptySetPermissions (* costruttore di un insieme di permessi vuoto *)
    | SingletonPermissions of expr (* costruttore di un insieme costituito da un solo elemento `expr` del tipo `tpermission` *)
    | OfPermissions of set_expr (* costruttore di un insieme di tipo `tpermission` a partire da una lista di espressioni `set_expr` *)
    (* Operazioni di base *)
    | Intersection of expr * expr (* intersezione di due insiemi *)
    | Difference of expr * expr (* differenza tra due insiemi *)
    (* Operazioni aggiuntive *)
    | Insert of expr * expr (* inserisce un elemento in un insieme *)
    | Remove of expr * expr (* rimuove un elemento da un insieme *)
    | Contains of expr * expr (* verifica se un insieme contiene un elemento *)
    | IsEmpty of expr (* verifica se un insieme è vuoto *)
    (* Operazioni fittizie per i test della stack inspection *)
    | ReadFile of expr * expr (* operazione di lettura di un file che prende 2 parametri: la risorsa da leggere e l'insieme di permessi *)
    and set_expr = 
        | Empty (* insieme di permessi vuoto *)
        | Cons of expr * set_expr (* oppure è il cons di un espressione e del resto dell'insieme dei permessi *)
    ;;


(* ambiente polimorfico *)
type 'v env = (ide * 'v  * 'v) list;;

(* valori a runtime *)
type evT = 
    | Int of int
    | String of string
    | Bool of bool 
    | Closure of ide * expr * evT env * ttype * ttype * expr
    (* ide: nome del parametro formale, expr: codice della funzione, evT env ambiente al momento della dichiarazione,
       ttype x2 : tipo in ingresso e in uscita della funzione *)
    
    (* introduco la RecClosure per la gestione della ricorsione *)
    | RecClosure of ide * ide * expr * evT env * ttype * ttype  * expr
    (* ide: nome della funzione, ide: nome del parametro formale, expr: codice della funzione, evT env ambiente al momento della dichiarazione,
       ttype x2 : tipo in ingresso e in uscita della funzione *)
    | Permission of evT * evT (* il costruttore di un nuovo Permission  prende una stringa per il tipo di permesso e una stringa per la risorsa *)
    | SetPermissions of set_val (* il costruttore di SetPermissions prende un valore dell'insieme *)
    | Unbound
    and set_val = 
        | EmptyV 
        | ConsV of evT * set_val;;


let empty_env : evT env  = [] ;;

let bind (env: evT env) (i: ide) (v: evT) (sop: evT) = ( i, v, sop ) :: env;;

let rec lookup (env: evT env) (i: ide) : evT = 
    match env with
    | [] ->  Unbound
    | (a,v,_)::_ when a = i -> v
    | _::e -> lookup e i;;

(*  type checking dinamico 
    typecheck ha tipo string -> evT -> bool, infatti ritorna bool
*)
let rec typecheck (t : string) (v : evT) : bool = 
match t with
    | "int" -> (match v with (* controllo se il valore passato è di tipo int *)
                | Int _ -> true
                | _ -> false )
    | "string" -> (match v with (* controllo se il valore passato è di tipo string *)
                | String _ -> true
                | _ -> false )
    | "bool" -> (match v with (* controllo se il valore passato è di tipo bool *)
                | Bool _ -> true
                | _ -> false )
    | "fun" -> (match v with   (* controllo se il valore passato è di tipo funval *)
                | Closure _ -> true
                | RecClosure _ -> true
                | _ -> false )
    | "permission" -> (match v with (* controllo se il valore passato è di tipo Permission*)
                       | Permission _ -> true
                       | _            -> false )
    | "setPermissions" -> (match v with   (* controllo se il valore passato è di tipo SetPermissions *)
                | SetPermissions _ -> true
                | _ -> false )
    | _ -> failwith "Typechecking dinamico fallito: la stringa non è corretta";;

    
(* gli passo uno dei tipi consentiti dal linguaggio e ritorna una funzione evt -> bool, sto facendo uso del currying 
   questa è una funzione ausiliaria che consente di effettuare una chiamata parametrizzata al typechecker dinamico sulla
   base del tipo passato come parametro *)
let check_from_lang_ttype (tt : ttype) : evT -> bool = 
match tt with
    | IntType                       -> typecheck "int"
    | StringType                    -> typecheck "string"
    | BoolType                      -> typecheck "bool"
    | FunType(_,_)                  -> typecheck "fun"
    | PermissionType(_)             -> typecheck "permission"
    | SetPermissionsType(_)         -> typecheck "setPermissions";;


(* Funzione che implementa la stack inspection *)

(* let rec stackInspect alist op perm =
  match alist with
  | Empty -> false
  | PAC(aop,pop,als)-> if op = aop && pop = perm then true else emCheck als op perm *)



(* funzioni ausiliarie *)
let int_add (n: evT) (m: evT) (*: evT *) =
    match typecheck "int" n, typecheck "int" m, n, m with
    | true, true, Int a, Int b -> Int(a + b)
    | _, _, _, _                  -> raise(DynamicTypeError "Add può essere chiamata solo su interi");;

let int_sub(n: evT) (m: evT) : evT = 
    match typecheck "int" n, typecheck "int" m, n, m with
    | true, true, Int a, Int b -> Int(a - b)
    | _,_,_,_                  -> raise(DynamicTypeError "Sub può essere chiamata solo su interi");;

let int_times(n: evT) (m: evT) : evT = 
    match typecheck "int" n, typecheck "int" m, n, m with
    | true, true, Int a, Int b -> Int (a * b)
    | _,_,_,_                  -> raise(DynamicTypeError "Times può essere chiamata solo su interi");;

let is_zero(n: evT) : evT = 
    match typecheck "int" n, n with
    | true, Int a -> Bool (a = 0)
    | _,_ -> raise(DynamicTypeError "IsZero può essere chiamata solo su interi");;

let int_equals(n: evT) (m: evT) : evT = 
    match typecheck "int" n, typecheck "int" m, n, m with
    | true, true, Int a, Int b -> Bool (a = b)
    | _,_,_,_                  -> raise(DynamicTypeError "Equals può essere chiamata solo su interi");;

let bool_and(n: evT) (m: evT) : evT = 
    match typecheck "bool" n, typecheck "bool" m, n, m with
    | true, true, Bool a, Bool b -> Bool (a && b)
    | _,_,_,_                  -> raise(DynamicTypeError "And può essere chiamata solo su booleani");;

let bool_or(n: evT) (m: evT) : evT = 
    match typecheck "bool" n, typecheck "bool" m, n, m with
    | true, true, Bool a, Bool b -> Bool (a || b)
    | _,_,_,_                  -> raise(DynamicTypeError "Or può essere chiamata solo su booleani");;

let bool_not(n: evT) : evT = 
    match typecheck "bool" n, n with
    | true, Bool a -> Bool (not a)
    | _,_                -> raise(DynamicTypeError "Not può essere chiamata solo su booleani");;

let string_equals(s: evT) (z: evT) : evT = 
    match typecheck "string" s, typecheck "string" z, s, z with
    | true, true, String a, String b -> Bool (a = b)
    | _,_,_,_                  -> raise(DynamicTypeError "EqualsString può essere chiamata solo su stringhe");;

let rec set_contains (s: set_val) (v: evT) : bool = 
    match s with
    | EmptyV -> false
    | ConsV (v', _) when v' = v -> true
    | ConsV (_,s') -> set_contains s' v;;

let rec set_union (s1: set_val) (s2: set_val) : set_val =
    match s1 with
    | EmptyV -> s2 (* se l'insieme s1 è vuoto ritorno s2 *)
    | ConsV (v1, s1') ->
                        if set_contains s2 v1 (* se l'insieme s2 contiene il valore in testa al cons *)
                        then set_union s1' s2 (* unisco solo la parte rimanente di s1 in quanto sto facendo l'unione *)
                        else set_union s1' (ConsV(v1,s2));; (* se l'insieme s2 non contiene il valore v1 allora lo aggiungo *)

let rec set_intersection (s1: set_val) (s2: set_val) : set_val = 
    match s1 with
    | EmptyV -> EmptyV
    | ConsV (v1,s1') ->
                        if set_contains s2 v1 (* se l'insieme s2 contiene il valore v1 allora ... *)
                        then ConsV(v1, (set_intersection s1' s2)) (* ritorno un insieme fatto da v1 e la chiamata ricorsiva, in quanto l'intersezione prende i valori comuni *)
                        else set_intersection s1' s2;; (* se il valore v1 non è comune ai 2 insiemi allora lo scarto *)

let rec set_difference (s1: set_val) (s2: set_val) : set_val = 
    match s1 with
    | EmptyV -> EmptyV
    | ConsV (v1,s1') ->
                        if set_contains s2 v1 (* se l'insieme s2 contiene il valore v1 allora ... *)
                        then set_difference s1' s2 (* scarto il valore v1 e continuo facendo una chiamata ricorsiva *)
                        else ConsV(v1, (set_difference s1' s2));; (* se il valore v1 non è in s2 allora lo mantengo *)

let rec set_remove (s: set_val) (v: evT) : set_val = 
    match s with
    | EmptyV -> EmptyV
    | ConsV (v',s') when v' = v -> s' (* ho trovato l'elemento che volevo rimuovere *)
    | ConsV (v',s') -> ConsV(v', set_remove s' v);; (* l'elemento v' non è quello che voglio rimuovere dunque lo mantengo *)


(* interprete *)
let rec eval (e: expr) (env: evT env) : evT = (* sop è il set of permissions, l'insieme contenente i permessi assegnati dal programmatore *)
    match e with
    | CstInt n -> Int n
    | CstTrue -> Bool true
    | CstFalse -> Bool false
    | CstString s -> String s
    | EqualsString (s, z) -> string_equals (eval s env) (eval z env) (* corrisponde all'uguaglianza tra stringhe *)
    | Den i -> lookup env i (* effettuo un lookup nell'ambiente env per l'identificatore i *)
    | Add (e1, e2) -> int_add (eval e1 env) (eval e2 env)(* corrisponde alla somma tra interi *)
    | Sub (e1, e2) -> int_sub (eval e1 env) (eval e2 env) (* corrisponde alla differenza tra interi *)
    | Times (e1, e2) -> int_times (eval e1 env) (eval e2 env) (* corrisponde al prodotto tra interi *)
    | IsZero e -> is_zero (eval e env)(* corrisponde a verificare se un intero ha valore 0 *)
    | Equals (e1, e2) -> int_equals (eval e1 env) (eval e2 env) (* corrisponde a verificare l'uguaglianza tra 2 espressioni intere *)
    | And (e1, e2) -> bool_and (eval e1 env) (eval e2 env) (* corrisponde all'and tra due espressioni *)
    | Or (e1, e2) -> bool_or (eval e1 env) (eval e2 env) (* corrisponde all'or tra due espressioni *)
    | Not e -> bool_not (eval e env) (* corrisponde alla negazione di un espressione *)
    | IfThenElse (guardia, e1, e2) -> (* corrisponde alla valutazione di un if then else *)
                                    (
                                        let evalCond = eval guardia env in (* valuto l'espressione della guardia... *)
                                            match typecheck "bool" evalCond, evalCond with (* prima valuto che la guardia sia di tipo bool... *)
                                            | true, Bool true -> eval e1 env (* se la guardia è booleana e il valore del ramo if è true, valuto il ramo if *)
                                            | true, Bool false -> eval e2 env (* se la guardia è booleana e il valore del ramo if è false, valuto il ramo else *)
                                            | _,_ -> raise (DynamicTypeError "La guardia dell' ifthenelse deve essere di tipo booleano") 
                                    )
    | Let (i, e1, e2, set_perm) ->
                                    (
                                        let val1 = eval e1 env in
                                        let setper_val = eval set_perm env in
                                        let env1 = bind env i (val1) setper_val in
                                        eval e2 env1 (* corrisponde alla introduzione di una nuova variabile *)
                                    )
    | Fun (i, body, t1, t2, set_perm) -> Closure (i, body, env, t1, t2, set_perm) (* corrisponde alla introduzione di una funzione: nome argomento, corpo, tipo argomento, tipo del ritorno *)
    | FunRec (funIde, p, t1, t2, bodyFun, bodyLet, set_perm) ->  (* corrisponde alla introduzione ricorsiva di funzioni *)
                                                    let recClosure = RecClosure (funIde, p, bodyFun, env, t1, t2, set_perm) in 
                                                    let setper_val = eval set_perm env in
                                                    let bindEnv = bind env funIde recClosure setper_val in
                                                    eval bodyLet bindEnv
    | Apply (funct, arg) ->  (* applicazione di una funzione *)
                            let funClosure = eval funct env in (* valuto la funzione nell'ambiente e vedo se è una closure o recursive closure... *)
                            (match funClosure with 
                            | Closure (param, bodyFun, declEnvFun, t1, _, set_perm) -> 
                                let actualVal = eval arg env in (* valuto il parametro ottenendo il valore attuale *)
                                if check_from_lang_ttype t1 actualVal = true
                                then (
                                        let setper_val = eval set_perm env in
                                        let actualEnv = bind declEnvFun param actualVal setper_val in
                                        eval bodyFun actualEnv (* valuto il body della funzione chiamata all'interno dell'ambiente di dichiarazione della
                                                                  funzione, con l'aggiunta del binding tra il parametro passato e il valore attuale calcolato *)
                                )
                                else raise (DynamicTypeError "Apply: ha fallito perchè il tipo del parametro attuale è errato")
                            | RecClosure (idFun, param, bodyFun, declEnvFun, t1, _, set_perm) -> 
                                let actualVal = eval arg env in (* valuto il parametro ottenendo il valore attuale *)
                                if check_from_lang_ttype t1 actualVal = true
                                then (
                                        let setper_val = eval set_perm env in
                                        let recEnv = bind declEnvFun idFun funClosure setper_val in 
                                (* l'ambiente ricorsivo è creato facendo il legame nell'ambiente al momento della dichiarazione della funzione 
                                   con il nome f e il suo valore. Quindi ci ricordiamo nell'ambiente di definizione che f è stata dichiarata
                                   esattamente in quell'ambiente ed è una funzione ricorsiva. Per una funzione ricorsiva infatti, nella closure
                                   il puntatore all'ambiente punta esattamente all'ambiente in cui questa è stata definita *)
                                        let actualEnv = bind recEnv param actualVal setper_val in (* ambiente di valutazione della funzione *)
                                        eval bodyFun actualEnv
                                )
                                else raise (DynamicTypeError "Apply: ha fallito perchè il tipo del parametro attuale è errato")
                            | _ -> raise (DynamicTypeError "Apply: ha fallito perchè il primo argomento non è una funzione")
                            )
    | NewPermission (action, resource) -> let v1 = eval action env in
                                          let v2 = eval resource env in
                                          if typecheck "string" v1 (* controllo che entrambi i parametri siano di tipo stringa, si potrebbe generalizzare considerando che potrebbe 
                                                                        trattarsi di qualsiasi oggetto (socket, connessione a un db, etc...) *)
                                          then Permission (v1, v2)
                                          else raise (DynamicTypeError "Il tipo dell'action del permission deve essere di tipo string")   
    (* Costruttori dell'insieme di permessi *)
    | EmptySetPermissions -> SetPermissions(EmptyV) (* costruttore di un insieme di permessi vuoto di tipo `ttype` *)
    | SingletonPermissions expr -> (* costruttore di un insieme di permessi costituito da un solo elemento `expr` del tipo `ttype` *)
                            let v = eval expr env in (* valuto l'espressione costituente l'insieme nell'ambiente *)
                            if typecheck "permission" v
                            then SetPermissions(ConsV(v,EmptyV))
                            else raise (DynamicTypeError "Il tipo del singleton non corrisponde con quello del valore")      
    | OfPermissions expr_list -> (* costruttore di un insieme di tipo `ttype` a partire da una lista di espressioni (un cons di espressioni o un espressione vuota) `set_expr` *)
                    let set_v = set_eval expr_list env in (* valuto la lista delle espressioni passate per trasformarla in una lista di valori dell'insieme *)
                                                        (* viene fatto anche il controllo dei tipi, grazie al t passato come parametro *)
                    SetPermissions(set_v)
    
    | Intersection (set_e1, set_e2) -> (* intersezione di due insiemi *)
                        (
                            match eval set_e1 env, eval set_e2 env with
                            | SetPermissions(set_v1), SetPermissions(set_v2) ->
                                                                                SetPermissions(set_intersection set_v1 set_v2) (* ritorno un SetPermissions di tipo t1=t2 e l'intersezione dei valori *)
                            | _, _ -> raise(DynamicTypeError "I tipi degli insiemi che si vogliono intersecare non combaciano")                            
                        ) 
    | Difference (set_e1, set_e2) -> (* differenza di due insiemi *)
                        (
                            match eval set_e1 env, eval set_e2 env with
                            | SetPermissions(set_v1), SetPermissions(set_v2) -> (* se i tipi dei 2 insiemi combaciano posso fare la differenza *)
                                                                                SetPermissions(set_difference set_v1 set_v2) (* ritorno un SetPermissions di tipo t1=t2 e la differenza dei valori *)
                            | _, _ -> raise(DynamicTypeError "I tipi degli insiemi di cui si vuole la differenza non combaciano")                            
                        )
    (* Operazioni aggiuntive *)
    | Insert (perm, set_perm) -> (* inserisce un elemento in un insieme *)
                        (
                            match eval set_perm env with (* valuto l'espressione che forma l'insieme... *)
                            | SetPermissions(set_v) ->  (* estraggo il tipo e i valori dell'insieme *)
                                                let v = eval perm env in (* valuto l'elemento da aggiungere ... *)
                                                if typecheck "permission" v && not (set_contains set_v v) (* il tipo dell'insieme e dell'elemento valutato coincidono 
                                                                                                              e l'insieme valutato non contiene il valore dell'elemento ...*)
                                                then SetPermissions(ConsV(v, set_v)) (* aggiungo l'elemento all'insieme *)
                                                else SetPermissions(set_v) (* altrimenti ritorna l'insieme come prima *)
                            | _ -> raise(DynamicTypeError "Insert: ha fallito perchè non è stata chiamata su un insieme")                            
                        ) 
    | Remove (perm, set_perm) -> (* rimuove un elemento in un insieme *)
                        (
                            match eval set_perm env with (* valuto l'espressione che forma l'insieme... *)
                            | SetPermissions(set_v) ->  (* estraggo il tipo e i valori dell'insieme *)
                                                let v = eval perm env in (* valuto l'elemento da aggiungere ... *)
                                                if typecheck "permission" v (* il tipo dell'insieme e dell'elemento valutato da rimuovere coincidono *)                                                               
                                                then SetPermissions(set_remove set_v v) (* rimuovo l'elemento dall'insieme *)
                                                else SetPermissions(set_v) (* altrimenti ritorna l'insieme come prima *)
                            | _ -> raise(DynamicTypeError "Remove: ha fallito perchè non è stata chiamata su un insieme")                            
                        ) 
    | Contains (perm, set_perm) -> (* verifica se un insieme contiene un elemento *)
                                (
                                    match eval set_perm env with
                                    | SetPermissions(set_v) -> (* estraggo il tipo e i valori dell'insieme... *)
                                                        let v = eval perm env in (* valuto l'argomento da aggiungere *)
                                                        if typecheck "permission" v (* il tipo dell'insieme e dell'elemento valutato da verificare coincidono... *)
                                                        then Bool (set_contains set_v v)
                                                        else raise(DynamicTypeError "Contains: ha fallito perchè i tipi dell'insieme e dell'elemento non combaciano")
                                    | _ -> raise(DynamicTypeError "Contains: ha fallito perchè non è stata chiamata su un insieme")  
                                )
    | IsEmpty set_perm -> (* verifica se un insieme è vuoto *)
                        (
                            match eval set_perm env with
                            | SetPermissions(set_v) -> 
                                                if set_v = EmptyV
                                                then Bool true
                                                else Bool false
                            | _ -> raise(DynamicTypeError "Impossibile chiamare IsEmpty su qualcosa diverso dall'insieme")                    
                        ) 
    | ReadFile(res, set_perm) -> (* la risorsa da leggere e il set di permessi *)
                        (
                            raise(DynamicTypeError "Qui andrà messa la funzione che fa lo stack inspection ma prima devo creare la struttura con stack per contenere tutte le chiamate a funzione attive")    
                        )
    (*il tipo dell'insieme dei permessi è sempre lo stesso quindi faccio un controllo hardcoded *)
    and set_eval (set_perm: set_expr) (env: evT env): set_val = 
        match set_perm with
        | Empty -> EmptyV (* se l'insieme è vuoto ritorna l'evaluation type dell'insieme vuoto *)
        | Cons (e, set_perm') -> (* decido di valutare prima il primo valore in testa così da evitare di chiamare la set_eval sul resto dell'insieme che è molto più grande in genere *)
                            let v = eval e env in
                            if typecheck "permission" v (* controllo il tipo *)
                            then
                                let set_v' = set_eval set_perm' env in (* poi valuto il resto dell'insieme *)
                                if set_contains set_v' v (* se il nuovo valore calcolato è contenuto nel resto dell'insieme calcolato *)
                                then set_v'              (*... non lo aggiunge dunque evito i duplicati! *)
                                else ConsV (v, set_v')   (*... altrimenti lo aggiunge tramite il cons *)
                            else raise(DynamicTypeError "Il tipo dell'insieme generato con of non corrisponde con quello del valore");;


(* === TESTS === *)
let env0 = empty_env;; (* Ambiente vuoto *)

(*  creo l'insieme di permessi contenente i permessi di lettura e scrittura sul file /etc/config e controllo se
    l'insieme di permessi creato contiene il permesso aggiunto poco prima *)

let test1 = eval ( 
                Let( "setperm1", 
                    OfPermissions ( Cons( NewPermission (CstString "write", CstString "/etc/config"), 
                                    Cons( NewPermission (CstString "read",CstString "/etc/config"), Empty) )
                                ),
                    Contains( NewPermission (CstString "read",CstString "/etc/config"), (* verifico se l'insieme dei permessi setperm1 contiene il permesso di lettura *)
                              Den("setperm1")
                            ),
                    EmptySetPermissions
                   )
) env0;;

assert (test1 = Bool true);;



(* creo 2 funzioni anonime: una con il permesso per leggere il file /etc/config e un altra che invece non ha il permesso 
   e chiama la prima per provare a bypassare il controllo, con la stack inspection questo non è possibile *)







(* testo la somma di 10 e 20
let result = eval ( Add(CstInt 10, CstInt 20) ) env0;;
assert (result = Int 30);;

(* testo la introduzione di una variabile var = 10 *)
let result = eval ( Let( "var", CstInt 10, Den("var") ) ) env0;;
assert (result = Int 10);;

(* testo la somma della variabile n=10 e della variabile m=30 *)
let result = eval ( Add( Let( "n", CstInt 10, Den("n") ) , Let( "m", CstInt 30, Den("m") ) ) ) env0;;
assert (result = Int 40);

(* testo la Apply con una funzione che incrementa di 2 l'argomento passato *)
let result = eval ( Apply ( Fun ( "x", Add ( Den "x", CstInt 2 ), IntType, IntType ), CstInt 2 ) ) env0 in
assert (result = Int 4);;

(* testo la Apply con una funzione che incrementa di 2 l'argomento passato e lo passa a un altra funzione che lo incrementa di 3 *)
let result = eval ( Apply ( Fun ( "y", Add ( Den "y", CstInt 3 ), IntType, IntType ), 
                        ( Apply ( Fun ( "x", Add ( Den "x", CstInt 2 ), IntType, IntType ), CstInt 2 ) )  ) ) env0 in
assert (result = Int 7);; *)


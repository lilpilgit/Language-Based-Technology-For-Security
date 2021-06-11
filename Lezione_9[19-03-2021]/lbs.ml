(*Partendo dal linguaggio, il codice intermedio, la generazione, eccetera eccetera. Abbiamo il linguaggio che è stato presentato finora, completo come vedete le espressioni sono quelle che avete già visto e sono state estese con due istruzioni aggiuntive che sono quella di read() e quella di send() e andremo a vedere perché poi nell'esempio. Allora, molto semplicemente la read() prende una stringa che rappresenta ad esempio il nome di un file, dispositivo di rete etc...e la send() invece prende un'altra espressione e una stringa e l'idea è quella di rappresentare con una stringa sempre un file o qualsiasi altra cosa e quello che viene mandato al file rappresentato dall' espressione. Abbiamo l'environment definito come avete già visto. L'idea è quella di dare la possibilità di rappresentare un automa, e costruire una funzione che compila l'automa, ovvero effettua l'inline dell'automa nel linguaggio.*)



type expr =
 | CstI of int
 | CstB of bool
 | Var of string
 | Let of string * expr * expr
 | Prim of string * expr * expr
 | If of expr * expr * expr
 | Fun of string * expr
 | Call of expr * expr
 | Read of string
 | Send of expr * string;;

(* An environment is a map from identifier to a value (what the identifier is bound to).
  For simplicity we represent the environment as an association list, i.e., a list of pair (identifier, data).
*)
type 'v env = (string * 'v) list;;

(**
  Given an environment {env} and an identifier {x} it returns the data {x} is bound to.
  If there is no binding, it raises an exception.
*)
let rec lookup env x =
   match env with
   | []        -> failwith (x ^ " not found")
   | (y, v)::r -> if x=y then v else lookup r x;;

(**
A runtime value is an integer or a function closure
Boolean are encoded as integers.
*)
(**type value =
 | Int of int
 | Closure of string * expr * value env;;
*)

(** 
Intermediate representation:
	expressions extended with Abort routine and global state manipulation
*)

type iexpr =
 | CstI of int
 | CstB of bool
 | Var of string
 | Let of string * iexpr * iexpr
 | Prim of string * iexpr * iexpr
 | If of iexpr * iexpr * iexpr
 | Fun of string * iexpr
 | Call of iexpr * iexpr
 | Read of string
 | Send of iexpr * string
 | Abort of string
 | GLet of iexpr * iexpr
 | GVar;;
 
 type ivalue =
 | Int of int
 | Closure of string * iexpr * ivalue env;;
 
 type gstate = GState of int;;
 
 (* Quindi abbiamo bisogno di rivedere in qualche modo la nostra funzione eval che interpreta il programma, perchè abbiamo anche questa nozione di stato globale, quindi, innanzitutto, la funzione prenderà un parametro aggiuntivo che è lo stato globale. Il valore di ritorno della funzione è non solo un tipo di dato value ma anche uno stato, quindi la funzione restituirà questa coppia, questo perché ad ogni passo dell interprete si può restituire un nuovo stato.*)
 let rec ieval (e : iexpr) (env : ivalue env) (g : gstate) : ivalue * gstate  =
   match e with
   (* Non cambia molto per diciamo i casi base, ovvero le costanti, le variabili e così via semplicemente lo stato che restituisco è quello iniziale, proprio perché le istruzioni non modificano questa variabile di stato globale.*)
   | CstI i -> (Int i, g)
   | CstB b -> (Int (if b then 1 else 0), g)
   | Var x  -> (lookup env x, g)
   (* Io ho assunto che la prima espressione che valuto, quindi il primo operando della funzione primitiva, possa cambiare lo stato e restituire uno stato gprime. Questo stato gprime è l' input della valutazione del secondo operando e lo stato che viene restituito è g''. Quindi valutare gli operandi della funzione primitiva può cambiare lo stato in linea di principio.*)
   | Prim(ope, e1, e2) ->
     let (v1, g') = ieval e1 env g in
     let (v2, g'') = ieval e2 env g' in
     begin
     match (ope, v1, v2) with
     | ("*", Int i1, Int i2) -> (Int (i1 * i2), g'')
     | ("+", Int i1, Int i2) -> (Int (i1 + i2), g'')
     | ("-", Int i1, Int i2) -> (Int (i1 - i2), g'')
     | ("=", Int i1, Int i2) -> (Int (if i1 = i2 then 1 else 0), g'')
     | ("<", Int i1, Int i2) -> (Int (if i1 < i2 then 1 else 0), g'')
     |  _ -> failwith "unknown primitive or wrong type"
     end
	 
	 (*Nel let la stessa cosa, se valuto il eRhs del let e questo non modifica lo stato ovvero ritorna g' questo stato g' prima viene poi passato alla valutazione del body e così via.*)
   | Let(x, eRhs, letBody) ->
     let (xVal, g') = ieval eRhs env g in
     let letEnv = (x, xVal) :: env in
     ieval letBody letEnv g'
   | If(e1, e2, e3) ->
     begin
     match ieval e1 env g with
     | (Int 0, g') -> ieval e3 env g'
     | (Int _, g') -> ieval e2 env g'
     | _     -> failwith "eval If"
     end
   | Fun(x,fBody) -> (Closure(x, fBody, env), g)
   | Call(eFun, eArg) ->
     let (fClosure, _) = ieval eFun env g in
     begin
     match fClosure with
     | Closure (x, fBody, fDeclEnv) ->
       let (xVal, g') = ieval eArg env g in
       let fBodyEnv = (x, xVal) :: fDeclEnv
       in ieval fBody fBodyEnv g'
     | _ -> failwith "eval Call: not a function"
     end
(**La Read e la Send() diciamo, non sono implementate nel senso che la Read() restituisce sempre zero e la Send() restituisce sempre uno. Questo perché non lo scopo non è quello di implementare la Read() e la Send() all'interno del linguaggio far vedere come fare inlining dell'automa. Poi la Read() non cambia lo stato, la Send() potrebbe, nel senso che valuto x, che è il primo parametro della Send() nello stato originale e se questa valutazione cambia lo stato in uno stato g' allora restituisco g'. Vediamo come si cambia lo stato. Glet è un let globale tra virgolette in cui a questa variabile di stato assegno il valore di letval, quindi quello che devo fare è valutare letval che è un'espressione. Se questa valutazione mi restituisce un intero a seguito del match, allora valuto il body del let in uno stato modificato che assume il valore dell'intero frutto della valutazione di letval. Se non è un intero, quindi, per esempio, una chiusura, allora diciamo, il programma non è ben formato, nel senso che sto assegnando allo stato una chiusura, e qesto non è previsto, essendo lo stato globale un intero. Come accediamo allo stato? Con gvar, che, semplicemente, corrisponde a estrarre l'intero corrispondente alla variabile globale. Quindi faccio un pattern matching su g estrapolo i che è il valore dello stato e restituisco un espressione contenente questo intero. L'idea di avere uno stato è proprio quella di tener traccia dell'automa. Avremmo potuto fare tutto nel linguaggio originale senza aggiungere questo glet e gvar ma sarebbe stato molto più complicato essendo il linguaggio originale funzionale, quindi passiamo per questa rappresentazione intermedia in cui possiamo modificare una variabile di stato globale e leggerla.**)
   | Read(_) -> (Int 0, g)
   | Send(x, _) -> let (_, g') = ieval x env g in (Int 1, g')
   | Abort msg -> failwith msg
   | GLet(letVal, letBody) -> let (xVal, _) = ieval letVal env g 
   								in begin match xVal with 
   									| Int(i) -> ieval letBody env (GState i)
   									| _ -> failwith "eval Call: not an integer"
   								end
   | GVar -> begin match g with
   			 	    | GState(i) -> (Int(i), g)
   			     end;;
   
(*Gli stati vengono rappresentati come interi, i simboli riconosciuti dall'automa sono espressioni della rappresentazione intermedia. E l'automa di per sè è rappresentato da una tupla contenente il primo stato, il secondo stato e la funzione di transizione, in questo caso particolare gli stati sono due, e si può rendere più generale, però per semplicità ha, per ora usiamo solo due stati.*)
type state = int;;
type symbol = iexpr;;
(* (s0, s1, delta, msg) *)

(*La funzione di transizione prende uno stato e un simbolo e restituisce uno state option, ovvero option è un tipo di dato polimorfo già presente in ocaml che può assumere o nessun valore None oppure un valore di tipo a in questo caso un valore di tipo state questo perché la funzione di transizione può essere parziale. In riferimento all'esempio della teoria, quando si era nel secondo stato e si leggeva una send() e non avveniva una transizione, la traccia veniva rifiutata. Infine il tipo di dato NFA prende anche una stringa, questa stringa rappresenta un messaggio di errore, diciamo nel momento in cui la funzione transizione non può essere eseguito, ovvero restituisce none. *)
type nfa = state * state * (state * symbol -> state option) * string;;

(* L'idea è quella di compilare l'automa all'interno del linguaggio di programmazione che abbiamo presentato finora. Quindi abbiamo bisogno di una rappresentazione intermedia che permette in qualche modo effettuare l' inline dell'automa all'interno del programma e questa rappresentazione intermedia. è data da iexpr che è il tipo di dato che estende le espressioni con ulteriori tre costrutti, che sono abort, glet e gvar. Per effettuare l' inline dell'automa abbiamo bisogno di rappresentare uno stato globale. Questo stato globale serve a tener conto dello stato in cui è l'automa, e quello verso cui effettuerà la transizione. Ci tengo a precisare che questi costrutti sono in linea di principio invisibile al programmatore, ovvero il programmatore scrive il programma sempre come un tipo di dato expr che è quello visto finora e una funzione di compilazione che vedremo dopo compila il tipo di dato expr in un tipo iexpr in cui sono incluse queste istruzioni nella maniera adeguata. abort è l'istruzione che termina il programma con un messaggio di errore. gvar restituisce il valore di uno stato globale. glet è simile a un let in cui non ho l' identificatore della variabile perché assumiamo che ce ne sia solo una, ci sia solo una variabile di stato globale, quindi glet assegna il valore di iexpr a questa variabile di stato globale e valuta il corpo del let in cui questa variabile di stato globale è stata modificata. Quello che noi stiamo considerando è un qualcosa che è nel backend del processo di compilazione, cioè chi programma usa la sintassi e qui stiamo dicendo che il programma noi abbiamo l'albero di sintassi astratta che è quello che ci viene da chi ha programmato poi chi ha programmato ha l'automa e lo definisce qui abbiamo fatto vedere un automa con due stati, Si vuole avere un meccanismo generale che astrae la problematica di come viene implementato la sicurezza dall'aspetto dichiarativo, che è quello che al programmatore interessa di dichiarare. Poi sarà il sistema a generare il codice instrumentato per fare in modo che questo codice instrumentato segua la politica di sicurezza che il programmatore ha definito l'automa. Il fatto che ci sia un meccanismo di codifica dello stato dell'automa all'interno del codice instrumentato non è sorprendente proprio per il fatto che noi stiamo facendo questa operazione di inlineing dell'automa. Ma se avessimo avuto l' il codice separato saremmo andati a eseguire in parallelo il programma con l'automa e l'automa sarebbe passato dallo stato uno allo stato due e lo sarebbe visto in parallelo, qui lo stiamo immergendo nel codice. Per questo Alessandro giustamente diceva, ci abbiamo un'unica variabile con lo stato uno e due, glet, quindi stiamo codificando come operare anche con questo stato, proprio perché ci occorre nella strumentazione.*)


(* type 'a option = None | Some of 'a;; *)

let inlineNfa (s0, s1, delta, msg) e = 
(**L'inline viene effettuata da questa funzione inlineNfa, che prende la specifica dell automa quindi lo stato S0, S1, la funzione transizione e il messaggio di errore con l'assunzione che lo stato iniziale è S0 e tutti gli stati sono di accettazione. La funzione inline prende anche un espressione che è quella su cui viene effettuato inline, quello che restituirà questa funzione è una serie di if then else in cui viene controllato che se sono nello stato S0 ed effettuo questa transizione, allora vado nello stato S1 e poi l'espressione su cui stiamo facendo inline. Definiamo questa catena di ifthenelse nel linguaggio che controlla esattamente questo, ovvero controlliamo che la variabile globale gVar sia uguale allo stato S0, che ricordo essere un intero. Se così fosse, allora vedo tramite la funzione transizione cosa leggendo l'espressione e nello stato S0 in quale stato finisco. Se non finisco in nessuno stato, ovvero la funzione non è definita per S0 ed e allora restituisco un messaggio di errore. Se invece la transizione è definita, allora vado nello stato S, quello che faccio è modificare la variabile di stato g con lo stato in cui mi manda la transizione. e dopo aver fatto questo assegnamento continuo con l'espressione originale. Se il sistema ha solo due stati S0 e S1 ,allora in realtà abbiamo un solo ifthenelse e questo else qui sotto è quello che valuta S1 e fa esattamente la stessa cosa. Qui si vede bene l'uso degli option Type. Allora l'automa è sempre accettante perché questa è la politica del security automa però essendo sempre accettante dobbiamo anche caratterizzare il caso in cui l'esecuzione di una certa azione, quello che è rappresentato dall espressione che fa un passaggio di stato, non abbia una controparte nella funzione di transizione e questo corrisponde esattamente al pattern matching che abbiamo qui con Some e None. Quando ci sta Some vuol dire che siamo in uno stato, stiamo eseguendo una espressione e questa espressione sta violando la politica di sicurezza definita dall'automa. Allora, dato che sta violando, abbattiamo e questo mi fa vedere esattamente il perché l'inline mi permette di mettere una valanga di ifthenelse in cascata all'interno del codice, e infatti viene messo prima dell'esecuzione, per evitare che l'esecuzione violi la politica di sicurezza in vigore. È chiaro che la viola soltanto quando la funzione di transizione non gli fa passare lo stato, quindi quando esattamente è il None degli option time, quindi vedete l'uso del None per codificare quelle situazioni quando non ho un valore buono tra virgolette, dove in questo caso il valore buono è un valore di Stato corretto, ovvero che un valore di stato che rispetta la politica che stiamo in questo momento considerando.**)
	If(Prim("=", GVar, CstI s0), 
		begin match (delta(s0,e)) with 
			| Some s -> GLet(CstI s, e)
			| None -> Abort(msg)
		end, 
		begin match (delta(s1,e)) with 
			| Some s -> GLet(CstI s, e)
			| None -> Abort(msg)
		end)
		
		
		

(**L'idea quella di scrivere una funzione comp che compila un espressione nel nostro linguaggio originale, in una espressione nella rappresentazione intermedia, ovvero in cui ho possibilità di accedere allo stato, modificarlo e così via, in base alla rappresentazione dell'automa. Secondo quello che si dice nell articolo originale di Erlingson, l'inline dell'automa viene effettuato prima dei branch delle chiamate, gli accessi memoria e così via. Vediamo come si fanno inline**)
let rec comp (sa : nfa) (p : expr) : iexpr = match p with
	| CstI i -> CstI i
	| CstB b -> CstB b
	| Var x -> inlineNfa sa (Var x)
	| Prim(ope, e1, e2) -> inlineNfa sa (Prim(ope, (comp sa e1), (comp sa e2)))
	| Let(x, e1, e2) -> Let(x, (comp sa e1), (comp sa e2))
	| If(e1, e2, e3) -> inlineNfa sa (If((comp sa e1), (comp sa e2), (comp sa e3)))
	| Fun(x, e) -> Fun(x, (comp sa e))
	| Call(f, a) -> inlineNfa sa (Call((comp sa f), (comp sa a)))
	| Read s -> inlineNfa sa (Read s)
	| Send(e, s) -> inlineNfa sa (Send((comp sa e), s));;

(**La funzione eval, che è quella che
valuta l'espressione originale
scritta dal
programmatore è
piuttosto semplificata,
nel senso che l'unica cosa che deve fare è
effettuare la valutazione della
rappresentazione intermedia,
ottenuta tramite la funzione di
compilazione in un ambiente che è
quello passato alla funzione e in
uno stato che è inizializzato a
initialState che è il primo elemento
della tupla rappresentante l'automa.
Quindi inizializzo lo stato globale
allo stato S0 dell'automa, compilo
il programma originale
in una rappresentazione intermedia
in cui è stato effettuato l'
inline dell'automa e valuto questa
rappresentazione intermedia.**)
let eval (e : expr) (env : ivalue env) (sa : nfa) : ivalue = match sa with
(**
Quello che restituisco è il risultato,
perché faccio questo let? Perché
ieval restituisce una coppia che è
il risultato e lo stato,
essendo che a me interessa solo il risultato,
butto via lo stato, diciamo.
**) 
	| (initialState, _, _, _) -> let (result, _) = ieval (comp sa e) env (GState initialState) in result;;
	
	
	
(**Qui definisco my Delta, ovvero la funzione di transizione dell'autonoma relativo alla politica: nessuna send dopo una read. Se sono nello stato 0 e leggo una read allora vado nello stato 1, c'è questo costruttore Some perché ricordo che la funzione transizione restituisce un tipo di dato option quindi non potrei mettere solo uno, ma devo costruirlo come tipo di dato option. Se sono nello stato zero e leggo qualsiasi altra cosa che non è una Read(), allora resto nello stato zero. Se sono nello stato uno e leggo una Send(), non mi interessano i parametri, allora vado in None, per questo caso la funzione transazione non è definita, quindi devo necessariamente restituire None. Se sono nello stato 1 e leggo qualsiasi altra cosa che non è una Send(), allora resto nello stato 1. Per qualsiasi altro caso restituisco None e questo caso del pattern matching è necessario, altrimenti l'interprete Ocamel non intepreta. Ecco anche qui e alesSend()o l'aveva già detto all'inizio mi permetto Qui stiamo venendo l'automa, ma sostanzialmente la rappresentazione in albero di sintassi astratta dell'automa, quello che dovreste dovete immaginare e che come c'è il fronted del compilatore che vedete, la sintassi completa, magari il programmatore scrive esattamente con i pallini e le freccine l'automa e poi è compilato dal frontend del compilatore nella sintassi intermedia, quindi, il programmato non è che vede questa parte qui, vede esattamente una visione friendly, dove poi è il sistema che si preoccuperà di trovare la rappresentazione intermedia. Quando facciamo i nostri esempi siamo dopo il frontend dei compilatori, non ci interessa come viene generato la rappresentazione intermedia.**)
let myDelta (s, e) = match (s, e) with
	| (0, Read _) -> Some 1
	| (0, _) -> Some 0
	| (1, Send(_, _)) -> None 
	| (1, _) -> Some 1
	| _ -> None;;

(** Quindi la definizione dell'automa prevede lo stato zero, lo stato uno e la funzione di transizione e come dicevo, il messaggio di errore per l'abort. Lo stato zero è quello iniziale perché per convenzione abbiamo deciso che il primo elemento della tupla è quello iniziale, tutti gli altri sono accettanti e anche lo stato zero.**)
let mySa = (0, 1, myDelta, "send after read detected");;
  
(**
Poi immaginiamo di scrivere un programma fatto da un solo let in cui x prende zero e poi viene valutato nella send di x al file uno. Questo programma è chiaramente safe rispetto alla policy che abbiamo definito perché prima della send non c'è alcuna read. Dalla funzione comp questo programma viene valutato in qualcosa che assomiglia a una cosa del genere:

let x = 0 in send(x, file1)

compiled into
Inizializza lo stato globale a zero. La Send() che ha il programma originale viene in qualche modo decorata da questo codice aggiuntivo in cui vengono effettuati i controlli necessari a far procedere l'automa, ovvero l'espressione originale Send() diventa qualcosa del genere: se sono allo stato zero, allora resto nello stato zero e faccio una send, altrimenti abort. Questo è esattamente il codice che viene fuori da la funzione inlineNfa o meglio da comp e più nello specifico da inlineNfa.
g = 0;
let x = 0 in (if g = 0 then g := 0; send(x, file1)
              else abort)
*)
let safeProgram : expr = Let("x", CstI 0, Send(Var "x", "file1"));;

(* comp mySa safeProgram;; 
DECOMMENTANDO LA ISTRUZIONE comp mySa safeProgram;; La rappresentazione di questo let nel nostro linguaggio è esattamente questa espressione come vedete un tipo di dato expr e se lo compilo quello che viene fuori è questo tipo di dato, iexpr in cui sono stati aggiunti tutti i controlli relativi alla variabile di stato globale. In particolare, il controllo che lo stato sia zero allora se è così assegno zero. Controllo che la variabile di Stato sia zero esattamente questa parte qui, se è così, allora nel nel ramo a Dan assegno. Zero aggi, cioè resto dello Stato zero e poi effettua la send em. Ci sono ulteriori controlli perché lì nell'auto, ma viene effettuato anche su l'espressione x, sulla variabile x, quindi che qui non ho riportato. Per semplicità, però ha la funzione comp fa anche questo e comunque nel primo if poi il ramo else e l' abort.
*)





(* eval safeProgram [] mySa;; 



DECOMMENTANDO L'ISTRUZIONE eval safeProgram [] mySa:


Abbiamo che se il programma
è un tipo di dato expr abbiamo l'ambiente
vuoto e abbiamo l'automa.
Quest'eval dovrebbe
effettuare l' inline dell'automa.
e poi valutarlo.
Effettivamente restituisce 1
perché il programma e safe, l'aborto non
è stato eseguito. 
E' chiaro che funziona perché
ho inizializzato g = 0,
quindi il ramo con "else abort" non viene
considerato.
*)

(**

Ora vediamo il caso unsafe. Quindi un programma in cui prima faccio una read da file2 per esempio e il valore di questa Read() corrisponderà a x nel let e poi valuto una send in cui mando il valore di x al file1, quindi leggo il file due questo valore lo mando al file due, chiaramente questo non rispetta la policy perché sto facendo una send dopo una Read(). Questo programma viene compilato come prima dalla funzione inline, la funzione comp in un programma simile a questo in cui: inizializza lo stato a zero, faccio i soliti controlli sull automa, in questo caso leggermente diversi perché l'espressione che leggo è una read e se sono nello stato 0, allora vado nello stato 1 e faccio la Read(), altrimenti abort. Proviamo a fare la valutazione di questo codice. Come vedete fare l'eval della versione compilata da cui si ottiene presentazione intermedia restituisce un eccezione che è esattamente la violazione della security policy. Questo perché è stato, diciamo eseguito, questo abort. L'inteprete ha riconosciuto che eravamo nello stato 1, quindi quando siamo andati a fare la Send() siamo andati nel secondo ramo della della ifthenelse, che prevedeva la abort. E notate che la Send() non è stata interpretata perché appunto l'abort ha terminato l'esecuzione del programma.


let x = read(file2) in send(x, file1)

compiled into
g = 0;
let x = (if g = 0 then g := 1; read(file2)
         else abort) in (if g = 0 then g := 0; send(x, file1)
                         else abort);
*)
let unsafeProgram : expr = Let("x", Read("file2"), Send(Var "x", "file1"));;

eval unsafeProgram [] mySa;;


(**
Il fatto di mettere delle guardie e dei codici di controllo è simile a quello che abbiamo visto quando abbiamo fatto vedere come è fatta la compilazione dei canary nel GCC che mettono delle guardie sul valore del canary che era un valore globale, quindi anche da questo punto di vista, poi l'osservazione che lo stato globale dell'implementazione, il gVal deve essere in un'area trusted non della trusted computed base, perché se l' attaccante va lì a modificare il valore dello Stato e quindi va a modificare quello che prima erano stato 1 in uno stato 0, si può in modo particolare violare la politica, quindi vuol dire che la trusted computed base deve essere fatta in modo che la il valore delle variabili di stato dell'automa devono essere in una parte che l' attaccante non può accedere esattamente come il valore delle canary, quindi stiamo tornando esattamente a stile di implementazione che abbiamo già visto quando abbiamo visto le canari su gcc.

**)